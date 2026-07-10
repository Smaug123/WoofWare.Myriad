namespace WoofWare.Myriad.Plugins.Test

open System.Text.Json.Nodes
open NUnit.Framework
open FsUnitTyped
open WoofWare.Myriad.Plugins.SwaggerV2

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
                        ResponseKey.Code 204, Definition.Unspecified
                        ResponseKey.Code 303, Definition.Unspecified
                        ResponseKey.Code 404, Definition.Unspecified
                    ]
                    |> Map.ofList
            }

    [<Test>]
    let ``Can parse inline response schemas`` () : unit =
        let s =
            """{
  "produces": [
    "application/json"
  ],
  "tags": [
    "repository"
  ],
  "summary": "Returns the names of the supported gitignore templates",
  "operationId": "listGitignoresTemplates",
  "responses": {
    "200": {
      "description": "GitignoreTemplateList",
      "schema": {
        "type": "array",
        "items": {
          "type": "string"
        }
      }
    },
    "403": {
      "$ref": "#/responses/forbidden"
    }
  }
}
"""
            |> JsonNode.Parse

        let endpoint = s.AsObject () |> SwaggerEndpoint.Parse

        endpoint.Responses
        |> shouldEqual (
            [
                ResponseKey.Code 200,
                Definition.Array
                    {
                        Items = Definition.String
                    }
                ResponseKey.Code 403, Definition.Handle "#/responses/forbidden"
            ]
            |> Map.ofList
        )

    [<Test>]
    let ``Can parse a default response`` () : unit =
        let s =
            """{
  "tags": [
    "pet"
  ],
  "summary": "Returns all pets from the system that the user has access to",
  "operationId": "findPets",
  "responses": {
    "200": {
      "description": "pet response",
      "schema": {
        "type": "string"
      }
    },
    "default": {
      "description": "unexpected error",
      "schema": {
        "$ref": "#/definitions/Error"
      }
    }
  }
}
"""
            |> JsonNode.Parse

        let endpoint = s.AsObject () |> SwaggerEndpoint.Parse

        endpoint.Responses
        |> shouldEqual (
            [
                ResponseKey.Code 200, Definition.String
                ResponseKey.Default, Definition.Handle "#/definitions/Error"
            ]
            |> Map.ofList
        )
