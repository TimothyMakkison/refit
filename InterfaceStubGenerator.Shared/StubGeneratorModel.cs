namespace Refit.Generator;

public record struct StubGeneratorModel(
    string PreserveAttributeSymbol,
    ImmutableEquatableArray<InterfaceModel> Interfaces
);

public record struct MethodModel(
    string Name,
    string ReturnType,
    string ReturnTypeMetadata,
    bool IsAsync,
    string ConfigureAwait,
    bool IsExplicit,
    bool IsTopLevel,
    ImmutableEquatableArray<ParameterModel> Parameters,
    ImmutableEquatableArray<GenericTypeModel> GenericTypes
);

public record struct ParameterModel(string Type, string Name, bool Annotation);


public record struct InterfaceModel(
    string ClassDeclaration,
    string ClassSuffix,
    string ContainingNamespace,
    string QualifiedName,
    bool SupportNullable,
    ImmutableEquatableArray<MethodModel> TopLevelRefitMethods,
    ImmutableEquatableArray<MethodModel> DerivedRefitMethods,
    ImmutableEquatableArray<MethodModel> NonRefitMethods,
    ImmutableEquatableArray<GenericTypeModel> GenericTypes,
    MethodModel? DisposeMethod
);
public record struct GenericTypeModel(
    string Name,
    ImmutableEquatableArray<string> Constraints
);
