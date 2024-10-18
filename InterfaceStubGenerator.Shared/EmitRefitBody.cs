using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis.Text;

namespace Refit.Generator;

internal static class EmitRefitBody
{
    private const string InnerMethodName = "Inner";
    private const string RequestName = "request";
    private const string SettingsExpression = "settings";

    public static void WriteRefitBody(StringBuilder source, RefitBodyModel model, HashSet<string> memberNames)
    {
        var innerMethodName = UniqueName(InnerMethodName, memberNames);
        var requestName = UniqueName(RequestName, memberNames);
        source.Append(
$$"""

              global::System.Net.Http.HttpRequestMessage {{innerMethodName}}()
              {
                  var {{requestName}} = global::System.Net.Http.HttpRequestMessage() { Method = global::System.Net.Http.HttpMethod.{{model.HttpMethod}} };

  """);

        TryWriteMultiPartInit(source, model, requestName);
        TryWriteBody(source, model, requestName);
        // need to run multi part attachment here.
        TryWriteHeaders(source, model, requestName);

        WriteProperties(source, model, requestName);
        WriteVersion(source, model, requestName);
    }

    private static void TryWriteMultiPartInit(StringBuilder source, RefitBodyModel model, string requestName)
    {
        if(model.MultipartBoundary is null)
            return;

        source.Append(
$$"""

                  {{requestName}}.Content = new global::System.Net.Http.MultipartFormDataContent({{model.MultipartBoundary}});
  """);
    }

    private static void TryWriteBody(StringBuilder source, RefitBodyModel model, string requestName)
    {
        if(model.BodyParameter is null)
            return;

        // TODO: use full alias for type
        source.Append(
            $$"""

                              Generated.AddBody({{requestName}}, {{model.BodyParameter.Parameter}}, {{model.BodyParameter.Buffered}}, {{model.BodyParameter.SerializationMethod}});
              """);
    }

    private static void TryWriteHeaders(StringBuilder source, RefitBodyModel model, string requestName)
    {
        // if no method headers, parameter headers or header collections don't emit
        if (model.Headers.Count == 0 && model.HeaderParameters.Count == 0)
        {
            if(model.HeaderCollectionParam is null)
                return;

            // TODO: ensure that AddHeaderCollection adds content
            source.Append(
                $$"""
                                  Generated.AddHeaderCollection({{requestName}}, {{model.HeaderCollectionParam}});
                  """);
            return;
        }

        // TODO: only emit if http method can have a body
        source.Append(
            $$"""
                              Generated.SetContentForHeaders({{requestName}}, );
              """);

        foreach (var methodHeader in model.Headers)
        {
            source.Append(
                $$"""
                                  Generated.AddHeader({{requestName}}, {{methodHeader.Key}}, {{methodHeader.Value}});
                  """);
        }

        foreach (var parameterHeader in model.HeaderParameters)
        {
            source.Append(
                $$"""
                                  Generated.AddHeader({{requestName}}, {{parameterHeader.HeaderKey}}, {{parameterHeader.Parameter}});
                  """);
        }
    }


    private static void WriteProperties(StringBuilder source, RefitBodyModel model, string requestName)
    {
        // add refit settings properties
        source.Append(
$$"""

                  Generated.WriteRefitSettingsProperties({{requestName}}, {{SettingsExpression}});
  """);

        // add each property
        foreach (var property in model.Properties)
        {
            source.Append(
                $$"""
                                  Generated.WriteProperty({{requestName}}, {{property.Key}}, {{property.Parameter}});
                  """);
        }

        // TODO: implement add top level types
        source.Append(
            $$"""

                              Generated.AddTopLevelTypes({{requestName}}, thisType, thisMethodInfo);
              """);
    }

    private static void WriteVersion(StringBuilder source, RefitBodyModel model, string requestName)
    {
        source.Append(
            $$"""
                              Generated.AddVersionToRequest({{requestName}}, {{SettingsExpression}});
              """);
    }

    private static void WriteBuildUrl(StringBuilder source, RefitBodyModel model, string requestName)
    {
        // add version to request
        source.Append(
            $$"""
                              Generated.AddVersionToRequest({{requestName}}, {{SettingsExpression}});
                              var vsb = new ValueStringBuilder(stackalloc char[256]);
              """);
    }

    private static string UniqueName(string name, HashSet<string> methodNames)
    {
        var candidateName = name;
        var counter = 0;
        while (methodNames.Contains(candidateName))
        {
            candidateName = $"{name}{counter}";
            counter++;
        }

        methodNames.Add(candidateName);
        return candidateName;
    }
}
