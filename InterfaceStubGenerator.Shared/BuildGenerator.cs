using System.Text;

using Microsoft.CodeAnalysis;

namespace Refit.Generator;

public static class BuildGenerator
{
    static string BuildInterface(
        InterfaceModel interfaceModel,
        string preserveAttribute,
        bool nullableEnabled
    )
    {
        var classDeclaration = interfaceModel.ClassDeclaration;
        var classSuffix = interfaceModel.ClassSuffix;

        // Get the class name itself
        // var classSuffix = $"{interfaceSymbol.ContainingType?.Name}{interfaceSymbol.Name}";
        // var ns = interfaceSymbol.ContainingNamespace?.ToDisplayString();

        // if it's the global namespace, our lookup rules say it should be the same as the class name
        // if(interfaceSymbol.ContainingNamespace != null && interfaceSymbol.ContainingNamespace.IsGlobalNamespace)
        // {
        //     ns = string.Empty;
        // }
        //
        // // Remove dots
        // ns = ns!.Replace(".", "");

        var ns = interfaceModel.ContainingNamespace;

        // Set the nullable context
        var source = new StringBuilder();
        if (interfaceModel.SupportNullable)
        {
            source.Append("#nullable ");

            source.Append(nullableEnabled ? "enable" : "disable");
        }

        source.Append(
            $@"
#pragma warning disable
namespace Refit.Implementation
{{

    partial class Generated
    {{

    /// <inheritdoc />
    [global::System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage]
    [global::System.Diagnostics.DebuggerNonUserCode]
    [{preserveAttribute}]
    [global::System.Reflection.Obfuscation(Exclude=true)]
    [global::System.ComponentModel.EditorBrowsable(global::System.ComponentModel.EditorBrowsableState.Never)]
    partial class {ns}{classDeclaration}
        : {interfaceModel.QualifiedName}{GenerateConstraints(interfaceModel.GenericTypes)}

    {{
        /// <inheritdoc />
        public global::System.Net.Http.HttpClient Client {{ get; }}
        readonly global::Refit.IRequestBuilder requestBuilder;

        /// <inheritdoc />
        public {ns}{classSuffix}(global::System.Net.Http.HttpClient client, global::Refit.IRequestBuilder requestBuilder)
        {{
            Client = client;
            this.requestBuilder = requestBuilder;
        }}

"
        );

        // Handle Refit Methods
        foreach (var method in interfaceModel.TopLevelRefitMethods)
        {
            ProcessRefitMethod(source, method, true, interfaceModel.QualifiedName);
        }

        foreach (
            var method in interfaceModel.TopLevelRefitMethods.Concat(
                interfaceModel.DerivedRefitMethods
            )
        )
        {
            ProcessRefitMethod(source, method, false, interfaceModel.QualifiedName);
        }

        // Handle non-refit Methods that aren't static or properties or have a method body
        foreach (var method in interfaceModel.NonRefitMethods)
        {
            ProcessNonRefitMethod(source, method, interfaceModel.QualifiedName);
        }

        // Handle Dispose if not null
        if (interfaceModel.DisposeMethod is { } disposeMethod)
        {
            ProcessDisposableMethod(source, disposeMethod, interfaceModel.QualifiedName);
        }

        source.Append(
            @"
    }
    }
}

#pragma warning restore
"
        );
        return source.ToString();
    }

    /// <summary>
    /// Generates the body of the Refit method
    /// </summary>
    /// <param name="source"></param>
    /// <param name="methodModel"></param>
    /// <param name="isTopLevel">True if directly from the type we're generating for, false for methods found on base interfaces</param>
    /// <param name="containingType">Name of the containing type.</param>
    static void ProcessRefitMethod(
        StringBuilder source,
        MethodModel methodModel,
        bool isTopLevel,
        string containingType
    )
    {
        var returnType = methodModel.ReturnType;
        var (isAsync, @return, configureAwait) = methodModel.ReturnTypeMetadata switch
        {
            "Task" => (true, "await (", ").ConfigureAwait(false)"),
            "Task`1" or "ValueTask`1" => (true, "return await (", ").ConfigureAwait(false)"),
            _ => (false, "return ", ""),
        };

        WriteMethodOpening(source, methodModel, !isTopLevel, containingType, isAsync);

        // List of generic arguments
        var genericList = methodModel.GenericTypes;
        var genericString =
            genericList.Count > 0
                ? $", new global::System.Type[] {{ {string.Join(", ", genericList)} }}"
                : string.Empty;

        source.Append(
            @$"
            var ______arguments = new object[] {{ {string.Join(", ", methodModel.Parameters.Select(x => x.Name))} }};
            var ______func = requestBuilder.BuildRestResultFuncForMethod(""{methodModel.Name}"", new global::System.Type[] {{ {string.Join(", ", methodModel.Parameters.Select(x => x.Type))} }}{genericString} );
            try
            {{
                {@return}({returnType})______func(this.Client, ______arguments){configureAwait};
            }}
            catch (global::System.Exception ex)
            {{
                throw ex;
            }}
"
        );

        WriteMethodClosing(source);
    }

    static void ProcessDisposableMethod(
        StringBuilder source,
        MethodModel methodModel,
        string containingType
    )
    {
        WriteMethodOpening(source, methodModel, true, containingType);

        source.Append(
            @"
                Client?.Dispose();
"
        );

        WriteMethodClosing(source);
    }

    static string GenerateConstraints(ImmutableEquatableArray<GenericTypeModel> typeParameters)
    {
        var source = new StringBuilder();
        // Need to loop over the constraints and create them
        foreach (var typeParameter in typeParameters)
        {
            WriteConstraitsForTypeParameter(source, typeParameter);
        }

        return source.ToString();
    }

    static void WriteConstraitsForTypeParameter(
        StringBuilder source,
        GenericTypeModel typeParameter
    )
    {
        if (typeParameter.Constraints.Count > 0)
        {
            source.Append(
                @$"
         where {typeParameter.Name} : {string.Join(", ", typeParameter.Constraints)}"
            );
        }
    }

    static void ProcessNonRefitMethod(
        StringBuilder source,
        MethodModel methodModel,
        string containingType
    )
    {
        WriteMethodOpening(source, methodModel, true, containingType);

        source.Append(
            @"
                throw new global::System.NotImplementedException(""Either this method has no Refit HTTP method attribute or you've used something other than a string literal for the 'path' argument."");
"
        );

        WriteMethodClosing(source);
    }

    static void WriteMethodOpening(
        StringBuilder source,
        MethodModel methodModel,
        bool isExplicitInterface,
        string containingType,
        bool isAsync = false
    )
    {
        var visibility = !isExplicitInterface ? "public " : string.Empty;
        var async = isAsync ? "async " : "";

        source.Append(
            @$"

        /// <inheritdoc />
        {visibility}{async}{methodModel.ReturnType} "
        );

        if (isExplicitInterface)
        {
            source.Append(@$"{containingType}.");
        }
        source.Append(@$"{methodModel.Name}(");

        if (methodModel.Parameters.Count > 0)
        {
            var list = new List<string>();
            foreach (var param in methodModel.Parameters)
            {
                var annotation = param.Annotation;

                list.Add($@"{param.Type}{(annotation ? '?' : string.Empty)} @{param.Name}");
            }

            source.Append(string.Join(", ", list));
        }

        source.Append(
            @$"){GenerateConstraints(methodModel.GenericTypes)}
        {{"
        );
    }

    static void WriteMethodClosing(StringBuilder source) => source.Append(@"        }");
}
