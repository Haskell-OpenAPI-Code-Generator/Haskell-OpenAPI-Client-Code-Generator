(import ./pkgs.nix).generateOpenAPIClient
{
  name = "stripe-api";
  src = ../specifications/stripe-api.yml;
  packageName = "stripe-api";
  moduleName = "StripeAPI";
  extraFlags = [
    "--property-type-suffix=\"'\""
    "--convert-to-camel-case"
  ];
  operations = [
    "GetEvents"
    "GetCustomers"
    "PostCheckoutSessions"
  ];
}
