using System.Collections.Immutable;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace Refit.Generator;

public class ParseGenerator
{
#pragma warning disable RS2008 // Enable analyzer release tracking
    static readonly DiagnosticDescriptor InvalidRefitMember = new(
        "RF001",
        "Refit types must have Refit HTTP method attributes",
        "Method {0}.{1} either has no Refit HTTP method attribute or you've used something other than a string literal for the 'path' argument",
        "Refit",
        DiagnosticSeverity.Warning,
        true);

    static readonly DiagnosticDescriptor RefitNotReferenced = new(
        "RF002",
        "Refit must be referenced",
        "Refit is not referenced. Add a reference to Refit.",
        "Refit",
        DiagnosticSeverity.Error,
        true);
#pragma warning restore RS2008 // Enable analyzer release tracking

    public void GenerateInterfaceStubs(
            Action<Diagnostic> reportDiagnostic,
            Action<string, SourceText> addSource,
            CSharpCompilation compilation,
            string? refitInternalNamespace,
            ImmutableArray<MethodDeclarationSyntax> candidateMethods,
            ImmutableArray<InterfaceDeclarationSyntax> candidateInterfaces)
        {
            refitInternalNamespace = $"{refitInternalNamespace ?? string.Empty}RefitInternalGenerated";

            // we're going to create a new compilation that contains the attribute.
            // TODO: we should allow source generators to provide source during initialize, so that this step isn't required.
            var options = (CSharpParseOptions)compilation.SyntaxTrees[0].Options;

            var disposableInterfaceSymbol = compilation.GetTypeByMetadataName("System.IDisposable")!;
            var httpMethodBaseAttributeSymbol = compilation.GetTypeByMetadataName("Refit.HttpMethodAttribute");

            if(httpMethodBaseAttributeSymbol == null)
            {
                reportDiagnostic(Diagnostic.Create(RefitNotReferenced, null));
                return;
            }


            // Check the candidates and keep the ones we're actually interested in

#pragma warning disable RS1024 // Compare symbols correctly
            var interfaceToNullableEnabledMap = new Dictionary<INamedTypeSymbol, bool>(SymbolEqualityComparer.Default);
#pragma warning restore RS1024 // Compare symbols correctly
            var methodSymbols = new List<IMethodSymbol>();
            foreach (var group in candidateMethods.GroupBy(m => m.SyntaxTree))
            {
                var model = compilation.GetSemanticModel(group.Key);
                foreach (var method in group)
                {
                    // Get the symbol being declared by the method
                    var methodSymbol = model.GetDeclaredSymbol(method);
                    if (IsRefitMethod(methodSymbol, httpMethodBaseAttributeSymbol))
                    {
                        var isAnnotated = compilation.Options.NullableContextOptions == NullableContextOptions.Enable ||
                            model.GetNullableContext(method.SpanStart) == NullableContext.Enabled;
                        interfaceToNullableEnabledMap[methodSymbol!.ContainingType] = isAnnotated;

                        methodSymbols.Add(methodSymbol!);
                    }
                }
            }

            var interfaces = methodSymbols.GroupBy<IMethodSymbol, INamedTypeSymbol>(m => m.ContainingType, SymbolEqualityComparer.Default)
                                          .ToDictionary(g => g.Key, v => v.ToList());

            // Look through the candidate interfaces
            foreach(var group in candidateInterfaces.GroupBy(i => i.SyntaxTree))
            {
                var model = compilation.GetSemanticModel(group.Key);
                foreach (var iface in group)
                {
                    // get the symbol belonging to the interface
                    var ifaceSymbol = model.GetDeclaredSymbol(iface);

                    // See if we already know about it, might be a dup
                    if (ifaceSymbol is null || interfaces.ContainsKey(ifaceSymbol))
                        continue;

                    // The interface has no refit methods, but its base interfaces might
                    var hasDerivedRefit = ifaceSymbol.AllInterfaces
                        .SelectMany(i => i.GetMembers().OfType<IMethodSymbol>())
                        .Any(m => IsRefitMethod(m, httpMethodBaseAttributeSymbol));

                    if (hasDerivedRefit)
                    {
                        // Add the interface to the generation list with an empty set of methods
                        // The logic already looks for base refit methods
                        interfaces.Add(ifaceSymbol, new List<IMethodSymbol>() );
                        var isAnnotated = model.GetNullableContext(iface.SpanStart) == NullableContext.Enabled;

                        interfaceToNullableEnabledMap[ifaceSymbol] = isAnnotated;
                    }
                }
            }

            // Bail out if there aren't any interfaces to generate code for. This may be the case with transitives
            if(interfaces.Count == 0) return;


            var supportsNullable = options.LanguageVersion >= LanguageVersion.CSharp8;

            var keyCount = new Dictionary<string, int>();

            var attributeText = @$"
#pragma warning disable
namespace {refitInternalNamespace}
{{
    [global::System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
    [global::System.ComponentModel.EditorBrowsable(global::System.ComponentModel.EditorBrowsableState.Never)]
    [global::System.AttributeUsage (global::System.AttributeTargets.Class | global::System.AttributeTargets.Struct | global::System.AttributeTargets.Enum | global::System.AttributeTargets.Constructor | global::System.AttributeTargets.Method | global::System.AttributeTargets.Property | global::System.AttributeTargets.Field | global::System.AttributeTargets.Event | global::System.AttributeTargets.Interface | global::System.AttributeTargets.Delegate)]
    sealed class PreserveAttribute : global::System.Attribute
    {{
        //
        // Fields
        //
        public bool AllMembers;

        public bool Conditional;
    }}
}}
#pragma warning restore
";


            compilation = compilation.AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(attributeText, Encoding.UTF8), options));

            // add the attribute text
            addSource("PreserveAttribute.g.cs", SourceText.From(attributeText, Encoding.UTF8));

            // get the newly bound attribute
            var preserveAttributeSymbol = compilation.GetTypeByMetadataName($"{refitInternalNamespace}.PreserveAttribute")!;

            var generatedClassText = @$"
#pragma warning disable
namespace Refit.Implementation
{{

    /// <inheritdoc />
    [global::System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
    [global::System.Diagnostics.DebuggerNonUserCode]
    [{preserveAttributeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}]
    [global::System.Reflection.Obfuscation(Exclude=true)]
    [global::System.ComponentModel.EditorBrowsable(global::System.ComponentModel.EditorBrowsableState.Never)]
    internal static partial class Generated
    {{
    }}
}}
#pragma warning restore
";
            addSource("Generated.g.cs", SourceText.From(generatedClassText, Encoding.UTF8));

            compilation = compilation.AddSyntaxTrees(CSharpSyntaxTree.ParseText(SourceText.From(generatedClassText, Encoding.UTF8), options));


            var interfaceModels = new List<InterfaceModel>();

            // group the fields by interface and generate the source
            foreach (var group in interfaces)
            {
                // each group is keyed by the Interface INamedTypeSymbol and contains the members
                // with a refit attribute on them. Types may contain other members, without the attribute, which we'll
                // need to check for and error out on
                var keyName = group.Key.Name;
                if(keyCount.TryGetValue(keyName, out var value))
                {
                    keyName = $"{keyName}{++value}";
                }
                keyCount[keyName] = value;

                var classSource = ProcessInterface(reportDiagnostic,
                                                   group.Key,
                                                   group.Value,
                                                   disposableInterfaceSymbol,
                                                   httpMethodBaseAttributeSymbol,
                                                   keyName,
                                                   supportsNullable,
                                                   interfaceToNullableEnabledMap[group.Key]);

                interfaceModels.Add(classSource);
            }

            var preserveAttributeSymbolString =
                preserveAttributeSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

            var generatorModel = new StubGeneratorModel(preserveAttributeSymbolString, interfaceModels.ToImmutableEquatableArray());
        }

        InterfaceModel ProcessInterface(Action<Diagnostic> reportDiagnostic,
                                INamedTypeSymbol interfaceSymbol,
                                List<IMethodSymbol> refitMethods,
                                ISymbol disposableInterfaceSymbol,
                                INamedTypeSymbol httpMethodBaseAttributeSymbol,
                                string fileName,
                                bool supportsNullable,
                                bool nullableEnabled)
        {

            // Get the class name with the type parameters, then remove the namespace
            var className = interfaceSymbol.ToDisplayString();
            var lastDot = className.LastIndexOf('.');
            if(lastDot > 0)
            {
                className = className.Substring(lastDot+1);
            }
            var classDeclaration = $"{interfaceSymbol.ContainingType?.Name}{className}";


            // Get the class name itself
            var classSuffix = $"{interfaceSymbol.ContainingType?.Name}{interfaceSymbol.Name}";
            var ns = interfaceSymbol.ContainingNamespace?.ToDisplayString();

            // if it's the global namespace, our lookup rules say it should be the same as the class name
            if(interfaceSymbol.ContainingNamespace != null && interfaceSymbol.ContainingNamespace.IsGlobalNamespace)
            {
                ns = string.Empty;
            }

            // Remove dots
            ns = ns!.Replace(".", "");

            // Get any other methods on the refit interfaces. We'll need to generate something for them and warn
            var nonRefitMethods = interfaceSymbol.GetMembers().OfType<IMethodSymbol>().Except(refitMethods, SymbolEqualityComparer.Default).Cast<IMethodSymbol>().ToList();

            // get methods for all inherited
            var derivedMethods = interfaceSymbol.AllInterfaces.SelectMany(i => i.GetMembers().OfType<IMethodSymbol>()).ToList();

            // Look for disposable
            var disposeMethod = derivedMethods.Find(m => m.ContainingType?.Equals(disposableInterfaceSymbol, SymbolEqualityComparer.Default) == true);
            if(disposeMethod != null)
            {
                //remove it from the derived methods list so we don't process it with the rest
                derivedMethods.Remove(disposeMethod);
            }

            // Pull out the refit methods from the derived types
            var derivedRefitMethods = derivedMethods.Where(m => IsRefitMethod(m, httpMethodBaseAttributeSymbol)).ToList();
            var derivedNonRefitMethods = derivedMethods.Except(derivedMethods, SymbolEqualityComparer.Default).Cast<IMethodSymbol>().ToList();

            // Handle Refit Methods
            var topLevelRefitMethods = new List<MethodModel>();
            foreach(var method in refitMethods)
            {
                topLevelRefitMethods.Add(ParseToMethodModel(method, true));
            }

            var derivedRefitMethodModels = new List<MethodModel>();
            foreach (var method in refitMethods.Concat(derivedRefitMethods))
            {
                derivedRefitMethodModels.Add(ParseToMethodModel( method, false));
            }

            var nonRefitMethodsModels = new List<MethodModel>();
            // Handle non-refit Methods that aren't static or properties or have a method body
            foreach (var method in nonRefitMethods.Concat(derivedNonRefitMethods))
            {
                if (method.IsStatic ||
                    method.MethodKind == MethodKind.PropertyGet ||
                    method.MethodKind == MethodKind.PropertySet ||
                    !method.IsAbstract) // If an interface method has a body, it won't be abstract
                    continue;

                nonRefitMethodsModels.Add(ProcessNonRefitMethod(reportDiagnostic, method));
            }

            var genericTypes = ProcessGenericTypeModel(interfaceSymbol.TypeParameters, false);

            // Handle Dispose
            MethodModel? disposeMethodModel = null;
            if(disposeMethod != null)
            {
                disposeMethodModel = ParseToMethodModel(disposeMethod, true);
            }

            var qualifiedName = interfaceSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

            return new InterfaceModel(fileName, classDeclaration, classSuffix, ns, qualifiedName,
                supportsNullable, nullableEnabled, topLevelRefitMethods.ToImmutableEquatableArray(),
                derivedRefitMethodModels.ToImmutableEquatableArray(), nonRefitMethodsModels.ToImmutableEquatableArray(),
                genericTypes, disposeMethodModel);

        }

        /// <summary>
        /// Generates the body of the Refit method
        /// </summary>
        /// <param name="source"></param>
        /// <param name="methodSymbol"></param>
        /// <param name="isTopLevel">True if directly from the type we're generating for, false for methods found on base interfaces</param>
        MethodModel ParseToMethodModel(IMethodSymbol methodSymbol, bool isTopLevel)
        {
            var returnType = methodSymbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var returnTypeMetadata = methodSymbol.ReturnType.MetadataName;

            var qualifiedMethodName = methodSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

            var parameters = ProcessParameterModel(methodSymbol);
            var generic = ProcessGenericTypeModel(methodSymbol.TypeParameters, !isTopLevel);

            return new MethodModel(methodSymbol.Name, qualifiedMethodName, returnType, returnTypeMetadata,
                parameters, generic);
        }

        static ImmutableEquatableArray<ParameterModel> ProcessParameterModel(IMethodSymbol methodSymbol)
        {
            return methodSymbol.Parameters.Select(p =>
            {
                var name = p.MetadataName;
                var type = p.Type.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
                var annotation = !p.Type.IsValueType && p.NullableAnnotation == NullableAnnotation.Annotated;
                return new ParameterModel(name, type, annotation);
            }).ToImmutableEquatableArray();
        }

        ImmutableEquatableArray<GenericTypeModel> ProcessGenericTypeModel(ImmutableArray<ITypeParameterSymbol> typeParameters, bool isOverrideOrExplicitImplementation)
        {
            return typeParameters.Select(p =>
            {
                var name = p.MetadataName;
                var constraints = WriteConstraitsForTypeParameter(p, isOverrideOrExplicitImplementation).ToImmutableEquatableArray();
                return new GenericTypeModel(name, constraints);
            }).ToImmutableEquatableArray();
        }

        List<string> WriteConstraitsForTypeParameter(ITypeParameterSymbol typeParameter, bool isOverrideOrExplicitImplementation)
        {
            // Explicit interface implementations and ovverrides can only have class or struct constraints

            var parameters = new List<string>();
            if(typeParameter.HasReferenceTypeConstraint)
            {
                parameters.Add("class");
            }
            if (typeParameter.HasUnmanagedTypeConstraint && !isOverrideOrExplicitImplementation)
            {
                parameters.Add("unmanaged");
            }
            if (typeParameter.HasValueTypeConstraint)
            {
                parameters.Add("struct");
            }
            if (typeParameter.HasNotNullConstraint && !isOverrideOrExplicitImplementation)
            {
                parameters.Add("notnull");
            }
            if (!isOverrideOrExplicitImplementation)
            {
                foreach (var typeConstraint in typeParameter.ConstraintTypes)
                {
                    parameters.Add(typeConstraint.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
                }
            }

            // new constraint has to be last
            if (typeParameter.HasConstructorConstraint && !isOverrideOrExplicitImplementation)
            {
                parameters.Add("new()");
            }

            return parameters;
        }

        MethodModel ProcessNonRefitMethod(Action<Diagnostic> reportDiagnostic,  IMethodSymbol methodSymbol)
        {
            foreach(var location in methodSymbol.Locations)
            {
                var diagnostic = Diagnostic.Create(InvalidRefitMember, location, methodSymbol.ContainingType.Name, methodSymbol.Name);
                reportDiagnostic(diagnostic);
            }

            return ParseToMethodModel(methodSymbol, true);
        }

        static bool IsRefitMethod(IMethodSymbol? methodSymbol, INamedTypeSymbol httpMethodAttibute)
        {
            return methodSymbol?.GetAttributes().Any(ad => ad.AttributeClass?.InheritsFromOrEquals(httpMethodAttibute) == true) == true;
        }
}
