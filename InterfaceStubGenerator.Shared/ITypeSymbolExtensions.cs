using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;

namespace Refit.Generator
{
    static class ITypeSymbolExtensions
    {
        public static IEnumerable<ITypeSymbol> GetBaseTypesAndThis(this ITypeSymbol? type)
        {
            var current = type;
            while (current != null)
            {
                yield return current;
                current = current.BaseType;
            }
        }

        // Determine if "type" inherits from "baseType", ignoring constructed types, optionally including interfaces,
        // dealing only with original types.
        public static bool InheritsFromOrEquals(
            this ITypeSymbol type,
            ITypeSymbol baseType,
            bool includeInterfaces
        )
        {
            if (!includeInterfaces)
            {
                return InheritsFromOrEquals(type, baseType);
            }

            return type.GetBaseTypesAndThis()
                .Concat(type.AllInterfaces)
                .Any(t => t.Equals(baseType, SymbolEqualityComparer.Default));
        }

        // Determine if "type" inherits from "baseType", ignoring constructed types and interfaces, dealing
        // only with original types.
        public static bool InheritsFromOrEquals(this ITypeSymbol type, ITypeSymbol baseType)
        {
            return type.GetBaseTypesAndThis()
                .Any(t => t.Equals(baseType, SymbolEqualityComparer.Default));
        }

        public static IEnumerable<AttributeData> GetAttributesFor(this ISymbol type, ITypeSymbol attributeType)
        {
            return type.GetAttributes().Where(t => t.AttributeClass!.InheritsFromOrEquals(attributeType));
        }

        public static T MapToType<T>(this AttributeData attributeData) where T : Attribute
        {
            T attribute;
            if (attributeData.AttributeConstructor != null && attributeData.ConstructorArguments.Length > 0)
            {
                attribute = (T) Activator.CreateInstance(typeof(T), attributeData.GetActualConstructorParams().ToArray());
            }
            else
            {
                attribute = (T) Activator.CreateInstance(typeof(T));
            }
            foreach (var p in attributeData.NamedArguments)
            {
                typeof(T).GetField(p.Key).SetValue(attribute, p.Value.Value);
            }
            return attribute;
        }

        public static IEnumerable<object> GetActualConstructorParams(this AttributeData attributeData)
        {
            foreach (var arg in attributeData.ConstructorArguments)
            {
                if (arg.Kind == TypedConstantKind.Array)
                {
                    // Assume they are strings, but the array that we get from this
                    // should actually be of type of the objects within it, be it strings or ints
                    // This is definitely possible with reflection, I just don't know how exactly.
                    yield return arg.Values.Select(a => a.Value).OfType<string>().ToArray();
                }
                else
                {
                    yield return arg.Value;
                }
            }
        }

        public static TAttribute? AccessFirstOrDefault<TAttribute>(this ISymbol symbol, WellKnownTypes knownTypes)
        where TAttribute : Attribute
        {
            var attributeSymbol = knownTypes.Get<TAttribute>();
            var attribute = symbol.GetAttributesFor(attributeSymbol).FirstOrDefault();
            return attribute?.MapToType<TAttribute>();
        }

        public static IEnumerable<TAttribute> Access<TAttribute>(this ISymbol symbol, WellKnownTypes knownTypes)
            where TAttribute : Attribute
        {
            var attributeSymbol = knownTypes.Get<TAttribute>();
            var attributes = symbol.GetAttributesFor(attributeSymbol);

            foreach (var attribute in attributes)
            {
                yield return attribute.MapToType<TAttribute>();
            }
        }
    }
}
