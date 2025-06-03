openapi-generator-cli generate \
  --input-spec genai_openapi.json \
  --generator-name haskell-http-client \
  --type-mappings AnyType=String \
  -p baseModule=GenAI.Client \
  -p cabalPackage=haskell-google-genai-client \
  -p configType=GenAIClientConfig \
  -p useKatip=false