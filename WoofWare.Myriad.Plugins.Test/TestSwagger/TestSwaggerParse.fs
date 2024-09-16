namespace WoofWare.Myriad.Plugins.Test

open System.Text.Json.Nodes
open NUnit.Framework
open FsUnitTyped
open WoofWare.Myriad.Plugins

[<TestFixture>]
module TestSwaggerParse =
    [<Test>]
    let ``Can parse parameters`` () : unit =
        let s =
            """{
  "tags": [
    "organization"
  ],
  "summary": "Check if a user is a member of an organization",
  "operationId": "orgIsMember",
  "parameters": [
    {
      "type": "string",
      "description": "name of the organization",
      "name": "org",
      "in": "path",
      "required": true
    },
    {
      "type": "string",
      "description": "username of the user",
      "name": "username",
      "in": "path",
      "required": true
    }
  ],
  "responses": {
    "204": {
      "description": "user is a member"
    },
    "303": {
      "description": "redirection to /orgs/{org}/public_members/{username}"
    },
    "404": {
      "description": "user is not a member"
    }
  }
}
"""
            |> JsonNode.Parse

        s.AsObject ()
        |> SwaggerEndpoint.Parse
        |> shouldEqual
            {
                Consumes = None
                Produces = None
                Tags = [ "organization" ]
                Summary = "Check if a user is a member of an organization"
                OperationId = OperationId "orgIsMember"
                Parameters =
                    [
                        {
                            Type = Definition.String
                            Description = Some "name of the organization"
                            Name = "org"
                            In = ParameterIn.Path "org"
                            Required = Some true
                        }
                        {
                            Type = Definition.String
                            Description = Some "username of the user"
                            Name = "username"
                            In = ParameterIn.Path "username"
                            Required = Some true
                        }
                    ]
                    |> Some
                Responses =
                    [
                        204, Definition.Unspecified
                        303, Definition.Unspecified
                        404, Definition.Unspecified
                    ]
                    |> Map.ofList
            }
