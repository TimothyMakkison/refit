using Microsoft.CodeAnalysis;

namespace Refit.Generator;

public class WellKnownTypes
{
    private readonly Compilation _compilation;
    private readonly Dictionary<string, INamedTypeSymbol?> _cachedTypes = new();

    public WellKnownTypes(Compilation compilation)
    {
        _compilation = compilation;
    }

    public INamedTypeSymbol Get<T>() => Get(typeof(T));
    public INamedTypeSymbol Get(Type type)
    {
        return Get(type.FullName ?? throw new InvalidOperationException("Could not get name of type " + type));
    }

    public INamedTypeSymbol? TryGet(string typeFullName)
    {
        if (_cachedTypes.TryGetValue(typeFullName, out var typeSymbol))
        {
            return typeSymbol;
        }

        typeSymbol = _compilation.GetTypeByMetadataName(typeFullName);
        _cachedTypes.Add(typeFullName, typeSymbol);

        return typeSymbol;
    }

    private INamedTypeSymbol Get(string typeFullName) =>
        TryGet(typeFullName) ?? throw new InvalidOperationException("Could not get type " + typeFullName);
}
