using System.Diagnostics;
using System.Reflection;
using System.Text.RegularExpressions;

using Microsoft.CodeAnalysis;

namespace Refit.Generator;

 /// <summary>
/// RestMethodInfo
/// </summary>
public record RestMethodInfo(
    string Name,
    Type HostingType,
    MethodInfo MethodInfo,
    string RelativePath,
    Type ReturnType
);

[DebuggerDisplay("{MethodInfo}")]
internal class RestMethodInfoInternal
{
    private int HeaderCollectionParameterIndex { get; set; }
    public string Name { get; set; }
    public Type Type { get; set; }
    public IMethodSymbol MethodInfo { get; set; }
    public HttpMethod HttpMethod { get; set; }
    public string RelativePath { get; set; }
    public bool IsMultipart { get; private set; }
    public string MultipartBoundary { get; private set; }
    public IParameterSymbol? CancellationToken { get; set; }
    public UriFormat QueryUriFormat { get; set; }
    public Dictionary<string, string?> Headers { get; set; }
    public Dictionary<int, string> HeaderParameterMap { get; set; }
    public Dictionary<int, string> PropertyParameterMap { get; set; }
    public Tuple<BodySerializationMethod, bool, int>? BodyParameterInfo { get; set; }
    public Tuple<string, int>? AuthorizeParameterInfo { get; set; }
    public Dictionary<int, string> QueryParameterMap { get; set; }
    public Dictionary<int, Tuple<string, string>> AttachmentNameMap { get; set; }
    public IParameterSymbol[] ParameterSymbolArray { get; set; }
    public Dictionary<int, RestMethodParameterInfo> ParameterMap { get; set; }
    public Type ReturnType { get; set; }
    public Type ReturnResultType { get; set; }
    public Type DeserializedResultType { get; set; }
    public RefitSettings RefitSettings { get; set; }
    public bool IsApiResponse { get; }
    public bool ShouldDisposeResponse { get; private set; }

    static readonly Regex ParameterRegex = new(@"{(.*?)}");
    static readonly HttpMethod PatchMethod = new("PATCH");

#pragma warning disable CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.
    public RestMethodInfoInternal(
        Type targetInterface,
        IMethodSymbol methodSymbol,
        WellKnownTypes knownTypes,
        RefitSettings? refitSettings = null
    )
#pragma warning restore CS8618 // Non-nullable field must contain a non-null value when exiting constructor. Consider declaring as nullable.
    {
        RefitSettings = refitSettings ?? new RefitSettings();
        Type = targetInterface ?? throw new ArgumentNullException(nameof(targetInterface));
        Name = methodSymbol.Name;
        MethodInfo = methodSymbol ?? throw new ArgumentNullException(nameof(methodSymbol));

        var hma = methodSymbol.AccessFirstOrDefault<HttpMethodAttribute>(knownTypes);

        HttpMethod = hma.Method;
        RelativePath = hma.Path;

        var multiPartsAttribute = methodSymbol.AccessFirstOrDefault<MultipartAttribute>(knownTypes);
        IsMultipart = multiPartsAttribute is not null;

        MultipartBoundary = IsMultipart
            ? multiPartsAttribute?.BoundaryText
              ?? new MultipartAttribute().BoundaryText
            : string.Empty;

        VerifyUrlPathIsSane(RelativePath);
        DetermineReturnTypeInfo(methodSymbol);
        DetermineIfResponseMustBeDisposed();

        // Exclude cancellation token parameters from this list
        var cancellationToken = knownTypes.Get<CancellationToken>();
        ParameterSymbolArray = methodSymbol
            .Parameters
            .Where(p => !SymbolEqualityComparer.Default.Equals(cancellationToken, p))
            .ToArray();
        ParameterMap = BuildParameterMap(RelativePath, ParameterSymbolArray);
        BodyParameterInfo = FindBodyParameter(ParameterSymbolArray, IsMultipart, hma.Method);
        AuthorizeParameterInfo = FindAuthorizationParameter(ParameterSymbolArray, knownTypes);

        Headers = ParseHeaders(methodSymbol, knownTypes);
        HeaderParameterMap = BuildHeaderParameterMap(ParameterSymbolArray, knownTypes);
        HeaderCollectionParameterIndex = RestMethodInfoInternal.GetHeaderCollectionParameterIndex(
            ParameterSymbolArray, knownTypes
        );
        PropertyParameterMap = BuildRequestPropertyMap(ParameterSymbolArray, knownTypes);

        // get names for multipart attachments
        Dictionary<int, Tuple<string, string>>? attachmentDict = null;
        if (IsMultipart)
        {
            for (var i = 0; i < ParameterSymbolArray.Length; i++)
            {
                if (
                    ParameterMap.ContainsKey(i)
                    || HeaderParameterMap.ContainsKey(i)
                    || PropertyParameterMap.ContainsKey(i)
                    || HeaderCollectionAt(i)
                )
                {
                    continue;
                }

                var attachmentName = GetAttachmentNameForParameter(ParameterSymbolArray[i]);
                if (attachmentName == null)
                    continue;

                attachmentDict ??= [];
                attachmentDict[i] = Tuple.Create(
                    attachmentName,
                    GetUrlNameForParameter(ParameterSymbolArray[i])
                );
            }
        }

        AttachmentNameMap = attachmentDict ?? new Dictionary<int, Tuple<string, string>>();

        Dictionary<int, string>? queryDict = null;
        for (var i = 0; i < ParameterSymbolArray.Length; i++)
        {
            if (
                ParameterMap.ContainsKey(i)
                || HeaderParameterMap.ContainsKey(i)
                || PropertyParameterMap.ContainsKey(i)
                || HeaderCollectionAt(i)
                || (BodyParameterInfo != null && BodyParameterInfo.Item3 == i)
                || (AuthorizeParameterInfo != null && AuthorizeParameterInfo.Item2 == i)
            )
            {
                continue;
            }

            queryDict ??= [];
            queryDict.Add(i, GetUrlNameForParameter(ParameterSymbolArray[i]));
        }

        QueryParameterMap = queryDict ?? EmptyDictionary<int, string>.Get();

        var ctParamEnumerable = methodSymbol
            .GetParameters()
            .Where(p => p.ParameterType == typeof(CancellationToken))
            .TryGetSingle(out var ctParam);
        if (ctParamEnumerable == EnumerablePeek.Many)
        {
            throw new ArgumentException(
                $"Argument list to method \"{methodSymbol.Name}\" can only contain a single CancellationToken"
            );
        }

        CancellationToken = ctParam;

        QueryUriFormat =  methodSymbol.GetCustomAttribute<QueryUriFormatAttribute>()?.UriFormat
                          ?? UriFormat.UriEscaped;

        IsApiResponse =
            ReturnResultType!.GetTypeInfo().IsGenericType
                && (
                    ReturnResultType!.GetGenericTypeDefinition() == typeof(ApiResponse<>)
                    || ReturnResultType.GetGenericTypeDefinition() == typeof(IApiResponse<>)
                )
            || ReturnResultType == typeof(IApiResponse);
    }

    public bool HasHeaderCollection => HeaderCollectionParameterIndex >= 0;

    public bool HeaderCollectionAt(int index) => HeaderCollectionParameterIndex >= 0 && HeaderCollectionParameterIndex == index;

    static int GetHeaderCollectionParameterIndex(IParameterSymbol[] parameterArray, WellKnownTypes knownTypes)
    {
        var headerIndex = -1;

        for (var i = 0; i < parameterArray.Length; i++)
        {
            var param = parameterArray[i];
            var headerCollection = param.AccessFirstOrDefault<HeaderCollectionAttribute>(knownTypes);

            if (headerCollection == null) continue;

            //opted for IDictionary<string, string> semantics here as opposed to the looser IEnumerable<KeyValuePair<string, string>> because IDictionary will enforce uniqueness of keys
            if (param.ParameterType.IsAssignableFrom(typeof(IDictionary<string, string>)))
            {
                // throw if there is already a HeaderCollection parameter
                if(headerIndex >= 0)
                    throw new ArgumentException("Only one parameter can be a HeaderCollection parameter");

                headerIndex = i;
            }
            else
            {
                throw new ArgumentException(
                    $"HeaderCollection parameter of type {param.ParameterType.Name} is not assignable from IDictionary<string, string>"
                );
            }
        }

        return headerIndex;
    }

    public RestMethodInfo ToRestMethodInfo() =>
        new(Name, Type, MethodInfo, RelativePath, ReturnType);

    static Dictionary<int, string> BuildRequestPropertyMap(IParameterSymbol[] parameterArray, WellKnownTypes knownTypes)
    {
        Dictionary<int, string>? propertyMap = null;

        for (var i = 0; i < parameterArray.Length; i++)
        {
            var param = parameterArray[i];
            var propertyAttribute = param
                .AccessFirstOrDefault<PropertyAttribute>(knownTypes);

            if (propertyAttribute != null)
            {
                var propertyKey = !string.IsNullOrEmpty(propertyAttribute.Key)
                    ? propertyAttribute.Key
                    : param.Name!;
                propertyMap ??= new Dictionary<int, string>();
                propertyMap[i] = propertyKey!;
            }
        }

        return propertyMap ?? new Dictionary<int, string>();
    }

    static IEnumerable<PropertyInfo> GetParameterProperties(ParameterInfo parameter)
    {
        return parameter
            .ParameterType.GetProperties(BindingFlags.Public | BindingFlags.Instance)
            .Where(static p => p.CanRead && p.GetMethod?.IsPublic == true);
    }

    static void VerifyUrlPathIsSane(string relativePath)
    {
        if (string.IsNullOrEmpty(relativePath))
            return;

        if (!relativePath.StartsWith("/"))
            throw new ArgumentException(
                $"URL path {relativePath} must start with '/' and be of the form '/foo/bar/baz'"
            );

        // CRLF injection protection
        if (relativePath.Contains('\r') || relativePath.Contains('\n'))
            throw new ArgumentException(
                $"URL path {relativePath} must not contain CR or LF characters"
            );
    }

    static Dictionary<int, RestMethodParameterInfo> BuildParameterMap(
        string relativePath,
        IParameterSymbol[] parameterSymbol,
        WellKnownTypes knownTypes
    )
    {
        var ret = new Dictionary<int, RestMethodParameterInfo>();

        // This section handles pattern matching in the URL. We also need it to add parameter key/values for any attribute with a [Query]
        var parameterizedParts = relativePath
            .Split('/', '?')
            .SelectMany(x => ParameterRegex.Matches(x).Cast<Match>())
            .ToList();

        if (parameterizedParts.Count > 0)
        {
            var paramValidationDict = parameterSymbol.ToDictionary(
                k => GetUrlNameForParameter(k, knownTypes).ToLowerInvariant(),
                v => v
            );
            //if the param is an lets make a dictionary for all it's potential parameters
            var objectParamValidationDict = parameterSymbol
                .Where(x => x.ParameterType.GetTypeInfo().IsClass)
                .SelectMany(x => GetParameterProperties(x).Select(p => Tuple.Create(x, p)))
                .GroupBy(
                    i => $"{i.Item1.Name}.{GetUrlNameForProperty(i.Item2)}".ToLowerInvariant()
                )
                .ToDictionary(k => k.Key, v => v.First());
            foreach (var match in parameterizedParts)
            {
                var rawName = match.Groups[1].Value.ToLowerInvariant();
                var isRoundTripping = rawName.StartsWith("**");
                string name;
                if (isRoundTripping)
                {
                    name = rawName.Substring(2);
                }
                else
                {
                    name = rawName;
                }

                if (paramValidationDict.TryGetValue(name, out var value)) //if it's a standard parameter
                {
                    var paramType = value.ParameterType;
                    if (isRoundTripping && paramType != typeof(string))
                    {
                        throw new ArgumentException(
                            $"URL {relativePath} has round-tripping parameter {rawName}, but the type of matched method parameter is {paramType.FullName}. It must be a string."
                        );
                    }
                    var parameterType = isRoundTripping
                        ? ParameterType.RoundTripping
                        : ParameterType.Normal;
                    var restMethodParameterInfo = new RestMethodParameterInfo(name, value)
                    {
                        Type = parameterType
                    };
#if NET6_0_OR_GREATER
                    ret.TryAdd(
                        Array.IndexOf(parameterInfo, restMethodParameterInfo.ParameterInfo),
                        restMethodParameterInfo
                    );
#else
                    var idx = Array.IndexOf(parameterSymbol, restMethodParameterInfo.ParameterInfo);
                    if (!ret.ContainsKey(idx))
                    {
                        ret.Add(idx, restMethodParameterInfo);
                    }
#endif
                }
                //else if it's a property on a object parameter
                else if (
                    objectParamValidationDict.TryGetValue(name, out var value1)
                    && !isRoundTripping
                )
                {
                    var property = value1;
                    var parameterIndex = Array.IndexOf(parameterSymbol, property.Item1);
                    //If we already have this parameter, add additional ParameterProperty
                    if (ret.TryGetValue(parameterIndex, out var value2))
                    {
                        if (!value2.IsObjectPropertyParameter)
                        {
                            throw new ArgumentException(
                                $"Parameter {property.Item1.Name} matches both a parameter and nested parameter on a parameter object"
                            );
                        }

                        value2.ParameterProperties.Add(
                            new RestMethodParameterProperty(name, property.Item2)
                        );
                    }
                    else
                    {
                        var restMethodParameterInfo = new RestMethodParameterInfo(
                            true,
                            property.Item1
                        );
                        restMethodParameterInfo.ParameterProperties.Add(
                            new RestMethodParameterProperty(name, property.Item2)
                        );
#if NET6_0_OR_GREATER
                        ret.TryAdd(
                            Array.IndexOf(parameterInfo, restMethodParameterInfo.ParameterInfo),
                            restMethodParameterInfo
                        );
#else
                        // Do the contains check
                        var idx = Array.IndexOf(parameterSymbol, restMethodParameterInfo.ParameterInfo);
                        if (!ret.ContainsKey(idx))
                        {
                            ret.Add(idx, restMethodParameterInfo);
                        }
#endif
                    }
                }
                else
                {
                    throw new ArgumentException(
                        $"URL {relativePath} has parameter {rawName}, but no method parameter matches"
                    );
                }
            }
        }
        return ret;
    }

    static string GetUrlNameForParameter(IParameterSymbol paramSymbol, WellKnownTypes knownTypes)
    {
        var aliasAttr = paramSymbol.AccessFirstOrDefault<AliasAsAttribute>(knownTypes);
        return aliasAttr != null ? aliasAttr.Name : paramSymbol.Name!;
    }

    static string GetUrlNameForProperty(IPropertySymbol propInfo, WellKnownTypes knownTypes)
    {
        var aliasAttr = propInfo.AccessFirstOrDefault<AliasAsAttribute>(knownTypes);
        return aliasAttr != null ? aliasAttr.Name : propInfo.Name;
    }

    static string GetAttachmentNameForParameter(ParameterInfo paramInfo)
    {
#pragma warning disable CS0618 // Type or member is obsolete
        var nameAttr = paramInfo
            .GetCustomAttributes<AttachmentNameAttribute>(true)
#pragma warning restore CS0618 // Type or member is obsolete
            .FirstOrDefault();

        // also check for AliasAs
        return nameAttr?.Name
            ?? paramInfo.GetCustomAttributes<AliasAsAttribute>(true).FirstOrDefault()?.Name!;
    }

    Tuple<BodySerializationMethod, bool, int>? FindBodyParameter(
        IParameterSymbol[] parameterArray,
        bool isMultipart,
        HttpMethod method,
        WellKnownTypes knownTypes
    )
    {
        // The body parameter is found using the following logic / order of precedence:
        // 1) [Body] attribute
        // 2) POST/PUT/PATCH: Reference type other than string
        // 3) If there are two reference types other than string, without the body attribute, throw

        var bodyParamEnumerable = parameterArray
            .Select(
                x =>
                (
                    Parameter: x,
                    BodyAttribute: x.AccessFirstOrDefault<BodyAttribute>();
                )
            )
            .Where(x => x.BodyAttribute != null)
            .TryGetSingle(out var bodyParam);

        // multipart requests may not contain a body, implicit or explicit
        if (isMultipart)
        {
            if (bodyParamEnumerable != EnumerablePeek.Empty)
            {
                throw new ArgumentException(
                    "Multipart requests may not contain a Body parameter"
                );
            }
            return null;
        }

        if (bodyParamEnumerable == EnumerablePeek.Many)
        {
            throw new ArgumentException("Only one parameter can be a Body parameter");
        }

        // #1, body attribute wins
        if (bodyParamEnumerable == EnumerablePeek.Single)
        {
            return Tuple.Create(
                bodyParam!.BodyAttribute!.SerializationMethod,
                bodyParam.BodyAttribute.Buffered ?? RefitSettings.Buffered,
                Array.IndexOf(parameterArray, bodyParam.Parameter)
            );
        }

        // Not in post/put/patch? bail
        if (
            !method.Equals(HttpMethod.Post)
            && !method.Equals(HttpMethod.Put)
            && !method.Equals(PatchMethod)
        )
        {
            return null;
        }

        // see if we're a post/put/patch
        // explicitly skip [Query], [HeaderCollection], and [Property]-denoted params
        var refParamEnumerable = parameterArray
            .Where(
                pi =>
                    !pi.ParameterType.GetTypeInfo().IsValueType
                    && pi.ParameterType != typeof(string)
                    && pi.GetCustomAttribute<QueryAttribute>() == null
                    && pi.GetCustomAttribute<HeaderCollectionAttribute>() == null
                    && pi.GetCustomAttribute<PropertyAttribute>() == null
            )
            .TryGetSingle(out var refParam);

        // Check for rule #3
        if (refParamEnumerable == EnumerablePeek.Many)
        {
            throw new ArgumentException(
                "Multiple complex types found. Specify one parameter as the body using BodyAttribute"
            );
        }

        if (refParamEnumerable == EnumerablePeek.Single)
        {
            return Tuple.Create(
                BodySerializationMethod.Serialized,
                RefitSettings.Buffered,
                Array.IndexOf(parameterArray, refParam!)
            );
        }

        return null;
    }

    static Tuple<string, int>? FindAuthorizationParameter(IParameterSymbol[] parameterArray, WellKnownTypes knownTypes)
    {
        var authorizeParams = parameterArray
            .Select(
                x =>
                (
                    Parameter: x,
                    AuthorizeAttribute: x.AccessFirstOrDefault<AuthorizeAttribute>(knownTypes)
                )
            )
            .Where(x => x.AuthorizeAttribute != null)
            .ToArray();

        if (authorizeParams.Length > 1)
        {
            throw new ArgumentException("Only one parameter can be an Authorize parameter");
        }

        if (authorizeParams.Length == 1)
        {
            var authorizeParam = authorizeParams.First();
            return Tuple.Create(
                authorizeParam!.AuthorizeAttribute!.Scheme,
                    Array.IndexOf(parameterArray, authorizeParam.Parameter)
            );
        }

        return null;
    }

    static Dictionary<string, string?> ParseHeaders(IMethodSymbol methodSymbol, WellKnownTypes knownTypes)
    {
        var inheritedAttributes =
            methodSymbol.DeclaringType != null
                ? methodSymbol
                    .DeclaringType.GetInterfaces()
                    .SelectMany(i => i.GetTypeInfo().GetCustomAttributes(true))
                    .Reverse()
                : [];

        var declaringTypeAttributes =
            methodSymbol.DeclaringType != null
                ? methodSymbol.DeclaringType.GetTypeInfo().GetCustomAttributes(true)
                : [];

        // Headers set on the declaring type have to come first,
        // so headers set on the method can replace them. Switching
        // the order here will break stuff.
        var headers = inheritedAttributes
            .Concat(declaringTypeAttributes)
            .Concat(methodSymbol.GetCustomAttributes(true))
            .OfType<HeadersAttribute>()
            .SelectMany(ha => ha.Headers);

        Dictionary<string, string?>? ret = null;

        foreach (var header in headers)
        {
            if (string.IsNullOrWhiteSpace(header))
                continue;

            ret ??= [];

            // NB: Silverlight doesn't have an overload for String.Split()
            // with a count parameter, but header values can contain
            // ':' so we have to re-join all but the first part to get the
            // value.
            var parts = header.Split(':');
            ret[parts[0].Trim()] =
                parts.Length > 1 ? string.Join(":", parts.Skip(1)).Trim() : null;
        }

        return ret ?? new Dictionary<string, string?>();
    }

    static Dictionary<int, string> BuildHeaderParameterMap(IParameterSymbol[] parameterArray, WellKnownTypes knownTypes)
    {
        Dictionary<int, string>? ret = null;

        for (var i = 0; i < parameterArray.Length; i++)
        {
            var headerAttribute = parameterArray[i]
                .AccessFirstOrDefault<HeaderAttribute>(knownTypes);

            var header = headerAttribute?.Header;

            if (!string.IsNullOrWhiteSpace(header))
            {
                ret ??= [];
                ret[i] = header.Trim();
            }
        }

        return ret ?? new Dictionary<int, string>();
    }

    void DetermineReturnTypeInfo(IMethodSymbol methodInfo)
    {
        var returnType = methodInfo.ReturnType;
        if (
            returnType.IsGenericType
            && (
                methodInfo.ReturnType.GetGenericTypeDefinition() == typeof(Task<>)
                || methodInfo.ReturnType.GetGenericTypeDefinition() == typeof(ValueTask<>)
                || methodInfo.ReturnType.GetGenericTypeDefinition() == typeof(IObservable<>)
            )
        )
        {
            ReturnType = returnType;
            ReturnResultType = returnType.GetGenericArguments()[0];

            if (
                ReturnResultType.IsGenericType
                && (
                    ReturnResultType.GetGenericTypeDefinition() == typeof(ApiResponse<>)
                    || ReturnResultType.GetGenericTypeDefinition() == typeof(IApiResponse<>)
                )
            )
            {
                DeserializedResultType = ReturnResultType.GetGenericArguments()[0];
            }
            else if (ReturnResultType == typeof(IApiResponse))
            {
                DeserializedResultType = typeof(HttpContent);
            }
            else
                DeserializedResultType = ReturnResultType;
        }
        else if (returnType == typeof(Task))
        {
            ReturnType = methodInfo.ReturnType;
            ReturnResultType = typeof(void);
            DeserializedResultType = typeof(void);
        }
        else
            throw new ArgumentException(
                $"Method \"{methodInfo.Name}\" is invalid. All REST Methods must return either Task<T> or ValueTask<T> or IObservable<T>"
            );
    }

    void DetermineIfResponseMustBeDisposed()
    {
        // Rest method caller will have to dispose if it's one of those 3
        ShouldDisposeResponse =
            DeserializedResultType != typeof(HttpResponseMessage)
            && DeserializedResultType != typeof(HttpContent)
            && DeserializedResultType != typeof(Stream);
    }
}
