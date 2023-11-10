namespace Refit.Generator;

public record struct StubGeneratorModel(
    string PreserveAttributeSymbol,
    ImmutableEquatableArray<InterfaceModel> Interfaces
);

public record struct MethodModel(
    string Name,
    string QualifiedName,
    string ReturnType,
    string ReturnTypeMetadata,
    ImmutableEquatableArray<ParameterModel> Parameters,
    ImmutableEquatableArray<GenericTypeModel> GenericTypes
);

public record struct ParameterModel(string Name, string Type, bool Annotation);


public record struct InterfaceModel(
    string FileName,
    string ClassDeclaration,
    string ClassSuffix,
    string ContainingNamespace,
    string QualifiedName,
    bool SupportNullable,
    bool NullableEnabled,
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
