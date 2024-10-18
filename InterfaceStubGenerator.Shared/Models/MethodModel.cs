namespace Refit.Generator;

internal sealed record MethodModel(
    string Name,
    string ReturnType,
    string ContainingType,
    string DeclaredMethod,
    ReturnTypeInfo ReturnTypeMetadata,
    ImmutableEquatableArray<ParameterModel> Parameters,
    ImmutableEquatableArray<TypeConstraint> Constraints
);

internal enum ReturnTypeInfo : byte
{
    Return,
    AsyncVoid,
    AsyncResult
}

internal record ThrowError(string errorExpression);

internal record RefitBodyModel(
    HttpMethod HttpMethod,
    string? cancellationTokenParam,
    string? MultipartBoundary,
    ImmutableEquatableArray<ParameterFragmentModel> PathFragments,
    ImmutableEquatableArray<HeaderModel> Headers,
    ImmutableEquatableArray<HeaderParameterModel> HeaderParameters,
    string? HeaderCollectionParam,
    ImmutableEquatableArray<AuthoriseModel> AuthoriseParameters,
    ImmutableEquatableArray<PropertyModel> Properties,
    ImmutableEquatableArray<QueryModel> QueryParameters,
    BodyModel? BodyParameter,
    UriFormat UriFormat
);

internal record struct HeaderModel(string Key, string Value);
internal record struct HeaderParameterModel(string Parameter, string HeaderKey);
internal record struct PropertyModel(string Parameter, string Key);
internal record struct AuthoriseModel(string Parameter, string Scheme);
internal record ParameterFragmentModel(string? Value, string? AccessExpression);
internal record QueryModel(string Parameter, CollectionFormat CollectionFormat, string Delimiter, string? Prefix, string? Format);
internal record BodyModel(string Parameter, bool Buffered, BodySerializationMethod SerializationMethod);


internal enum BodyParameterType
{
    Content,
    Stream,
    String,
}

internal enum ParamFrag
{
    Constant, Parameter, ParameterProperty
}
