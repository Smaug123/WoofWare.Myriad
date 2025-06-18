namespace WoofWare.Myriad.Plugins.Test

open WoofWare.Myriad.Plugins.OpenApi3
open System.Text.Json.Nodes
open NUnit.Framework
open WoofWare.Expect

[<TestFixture>]
[<Parallelizable(ParallelScope.Children)>]
module TestOpenApi3Parse =
    [<OneTimeSetUp>]
    let ``Prepare to bulk-update tests`` () =
        GlobalBuilderConfig.enterBulkUpdateMode ()


    [<OneTimeTearDown>]
    let ``Update all tests`` () =
        GlobalBuilderConfig.updateAllSnapshots ()

    type Dummy = class end

    [<Test>]
    let ``API with examples`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "api-with-examples.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.0.0"",
  ""Info"": {
    ""Title"": ""Simple API overview"",
    ""Description"": null,
    ""TermsOfService"": null,
    ""Contact"": null,
    ""License"": null,
    ""Version"": ""2.0.0""
  },
  ""Servers"": null,
  ""Paths"": {
    ""Fields"": {
      ""/"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": ""List API versions"",
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""listVersionsv2"",
          ""Parameters"": null,
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""200 response"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": null,
                        ""Example"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""foo"": {
                                ""Case"": ""Choice1Of2"",
                                ""Fields"": [
                                  {
                                    ""Summary"": null,
                                    ""Description"": null,
                                    ""Value"": {
                                      ""Case"": ""Choice1Of2"",
                                      ""Fields"": [
                                        {
                                          ""versions"": [
                                            {
                                              ""status"": ""CURRENT"",
                                              ""updated"": ""2011-01-21T11:33:21Z"",
                                              ""id"": ""v2.0"",
                                              ""links"": [
                                                {
                                                  ""href"": ""http://127.0.0.1:8774/v2/"",
                                                  ""rel"": ""self""
                                                }
                                              ]
                                            },
                                            {
                                              ""status"": ""EXPERIMENTAL"",
                                              ""updated"": ""2013-07-23T11:33:21Z"",
                                              ""id"": ""v3.0"",
                                              ""links"": [
                                                {
                                                  ""href"": ""http://127.0.0.1:8774/v3/"",
                                                  ""rel"": ""self""
                                                }
                                              ]
                                            }
                                          ]
                                        }
                                      ]
                                    }
                                  }
                                ]
                              }
                            }
                          ]
                        },
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              },
              ""300"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""300 response"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": null,
                        ""Example"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""foo"": {
                                ""Case"": ""Choice1Of2"",
                                ""Fields"": [
                                  {
                                    ""Summary"": null,
                                    ""Description"": null,
                                    ""Value"": {
                                      ""Case"": ""Choice1Of2"",
                                      ""Fields"": [
                                        {
                                          ""versions"": [
                                            {
                                              ""status"": ""CURRENT"",
                                              ""updated"": ""2011-01-21T11:33:21Z"",
                                              ""id"": ""v2.0"",
                                              ""links"": [
                                                {
                                                  ""href"": ""http://127.0.0.1:8774/v2/"",
                                                  ""rel"": ""self""
                                                }
                                              ]
                                            },
                                            {
                                              ""status"": ""EXPERIMENTAL"",
                                              ""updated"": ""2013-07-23T11:33:21Z"",
                                              ""id"": ""v3.0"",
                                              ""links"": [
                                                {
                                                  ""href"": ""http://127.0.0.1:8774/v3/"",
                                                  ""rel"": ""self""
                                                }
                                              ]
                                            }
                                          ]
                                        }
                                      ]
                                    }
                                  }
                                ]
                              }
                            }
                          ]
                        },
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/v2"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": ""Show API version details"",
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""getVersionDetailsv2"",
          ""Parameters"": null,
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""200 response"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": null,
                        ""Example"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""foo"": {
                                ""Case"": ""Choice1Of2"",
                                ""Fields"": [
                                  {
                                    ""Summary"": null,
                                    ""Description"": null,
                                    ""Value"": {
                                      ""Case"": ""Choice1Of2"",
                                      ""Fields"": [
                                        {
                                          ""version"": {
                                            ""status"": ""CURRENT"",
                                            ""updated"": ""2011-01-21T11:33:21Z"",
                                            ""media-types"": [
                                              {
                                                ""base"": ""application/xml"",
                                                ""type"": ""application/vnd.openstack.compute\u002Bxml;version=2""
                                              },
                                              {
                                                ""base"": ""application/json"",
                                                ""type"": ""application/vnd.openstack.compute\u002Bjson;version=2""
                                              }
                                            ],
                                            ""id"": ""v2.0"",
                                            ""links"": [
                                              {
                                                ""href"": ""http://127.0.0.1:8774/v2/"",
                                                ""rel"": ""self""
                                              },
                                              {
                                                ""href"": ""http://docs.openstack.org/api/openstack-compute/2/os-compute-devguide-2.pdf"",
                                                ""type"": ""application/pdf"",
                                                ""rel"": ""describedby""
                                              },
                                              {
                                                ""href"": ""http://docs.openstack.org/api/openstack-compute/2/wadl/os-compute-2.wadl"",
                                                ""type"": ""application/vnd.sun.wadl\u002Bxml"",
                                                ""rel"": ""describedby""
                                              },
                                              {
                                                ""href"": ""http://docs.openstack.org/api/openstack-compute/2/wadl/os-compute-2.wadl"",
                                                ""type"": ""application/vnd.sun.wadl\u002Bxml"",
                                                ""rel"": ""describedby""
                                              }
                                            ]
                                          }
                                        }
                                      ]
                                    }
                                  }
                                ]
                              }
                            }
                          ]
                        },
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              },
              ""203"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""203 response"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": null,
                        ""Example"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""foo"": {
                                ""Case"": ""Choice1Of2"",
                                ""Fields"": [
                                  {
                                    ""Summary"": null,
                                    ""Description"": null,
                                    ""Value"": {
                                      ""Case"": ""Choice1Of2"",
                                      ""Fields"": [
                                        {
                                          ""version"": {
                                            ""status"": ""CURRENT"",
                                            ""updated"": ""2011-01-21T11:33:21Z"",
                                            ""media-types"": [
                                              {
                                                ""base"": ""application/xml"",
                                                ""type"": ""application/vnd.openstack.compute\u002Bxml;version=2""
                                              },
                                              {
                                                ""base"": ""application/json"",
                                                ""type"": ""application/vnd.openstack.compute\u002Bjson;version=2""
                                              }
                                            ],
                                            ""id"": ""v2.0"",
                                            ""links"": [
                                              {
                                                ""href"": ""http://23.253.228.211:8774/v2/"",
                                                ""rel"": ""self""
                                              },
                                              {
                                                ""href"": ""http://docs.openstack.org/api/openstack-compute/2/os-compute-devguide-2.pdf"",
                                                ""type"": ""application/pdf"",
                                                ""rel"": ""describedby""
                                              },
                                              {
                                                ""href"": ""http://docs.openstack.org/api/openstack-compute/2/wadl/os-compute-2.wadl"",
                                                ""type"": ""application/vnd.sun.wadl\u002Bxml"",
                                                ""rel"": ""describedby""
                                              }
                                            ]
                                          }
                                        }
                                      ]
                                    }
                                  }
                                ]
                              }
                            }
                          ]
                        },
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      }
    }
  },
  ""Components"": null,
  ""Security"": null,
  ""Tags"": null,
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``Callback example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "callback-example.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.0.0"",
  ""Info"": {
    ""Title"": ""Callback Example"",
    ""Description"": null,
    ""TermsOfService"": null,
    ""Contact"": null,
    ""License"": null,
    ""Version"": ""1.0.0""
  },
  ""Servers"": null,
  ""Paths"": {
    ""Fields"": {
      ""/streams"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": null,
        ""Put"": null,
        ""Post"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": ""subscribes a client to receive out-of-band data"",
          ""ExternalDocs"": null,
          ""OperationId"": null,
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""callbackUrl"",
                  ""In"": {
                    ""Case"": ""Query""
                  },
                  ""Description"": ""the location where data will be sent.  Must be network accessible\nby the source server\n"",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""201"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""subscription successfully created"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            null
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": {
            ""onData"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Patterns"": {
                    ""{$request.query.callbackUrl}/data"": {
                      ""Ref"": null,
                      ""Summary"": null,
                      ""Description"": null,
                      ""Get"": null,
                      ""Put"": null,
                      ""Post"": {
                        ""Tags"": null,
                        ""Summary"": null,
                        ""Description"": null,
                        ""ExternalDocs"": null,
                        ""OperationId"": null,
                        ""Parameters"": null,
                        ""RequestBody"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            {
                              ""Description"": ""subscription payload"",
                              ""Content"": {
                                ""application/json"": {
                                  ""Schema"": {
                                    ""Case"": ""Choice1Of2"",
                                    ""Fields"": [
                                      null
                                    ]
                                  },
                                  ""Example"": null,
                                  ""Encoding"": null
                                }
                              },
                              ""Required"": null
                            }
                          ]
                        },
                        ""Responses"": {
                          ""Default"": null,
                          ""Patterns"": {
                            ""202"": {
                              ""Case"": ""Choice1Of2"",
                              ""Fields"": [
                                {
                                  ""Description"": ""Your server implementation should return this HTTP status code\nif the data was received successfully\n"",
                                  ""Headers"": null,
                                  ""Content"": null,
                                  ""Links"": null
                                }
                              ]
                            },
                            ""204"": {
                              ""Case"": ""Choice1Of2"",
                              ""Fields"": [
                                {
                                  ""Description"": ""Your server should return this HTTP status code if no longer interested\nin further updates\n"",
                                  ""Headers"": null,
                                  ""Content"": null,
                                  ""Links"": null
                                }
                              ]
                            }
                          }
                        },
                        ""Callbacks"": null,
                        ""Deprecated"": null,
                        ""Security"": null,
                        ""Servers"": null
                      },
                      ""Delete"": null,
                      ""Options"": null,
                      ""Head"": null,
                      ""Patch"": null,
                      ""Trace"": null,
                      ""Servers"": null,
                      ""Parameters"": null
                    }
                  }
                }
              ]
            }
          },
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      }
    }
  },
  ""Components"": null,
  ""Security"": null,
  ""Tags"": null,
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``Link example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "link-example.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.0.0"",
  ""Info"": {
    ""Title"": ""Link Example"",
    ""Description"": null,
    ""TermsOfService"": null,
    ""Contact"": null,
    ""License"": null,
    ""Version"": ""1.0.0""
  },
  ""Servers"": null,
  ""Paths"": {
    ""Fields"": {
      ""/2.0/repositories/{username}"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""getRepositoriesByOwner"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""username"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""repositories owned by the supplied user"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            null
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": {
                      ""userRepository"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/links/UserRepository""
                          }
                        ]
                      }
                    }
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/2.0/repositories/{username}/{slug}"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""getRepository"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""username"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""slug"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""The repository"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/repository""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": {
                      ""repositoryPullRequests"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/links/RepositoryPullRequests""
                          }
                        ]
                      }
                    }
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/2.0/repositories/{username}/{slug}/pullrequests"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""getPullRequestsByRepository"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""username"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""slug"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""state"",
                  ""In"": {
                    ""Case"": ""Query""
                  },
                  ""Description"": null,
                  ""Required"": null,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""an array of pull request objects"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            null
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/2.0/repositories/{username}/{slug}/pullrequests/{pid}"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""getPullRequestsById"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""username"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""slug"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""pid"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""a pull request object"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/pullrequest""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": {
                      ""pullRequestMerge"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/links/PullRequestMerge""
                          }
                        ]
                      }
                    }
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/2.0/repositories/{username}/{slug}/pullrequests/{pid}/merge"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": null,
        ""Put"": null,
        ""Post"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""mergePullRequest"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""username"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""slug"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""pid"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""204"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""the PR was successfully merged"",
                    ""Headers"": null,
                    ""Content"": null,
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/2.0/users/{username}"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""getUserByName"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""username"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": null,
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""The User"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/user""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": {
                      ""userRepositories"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/links/UserRepositories""
                          }
                        ]
                      }
                    }
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      }
    }
  },
  ""Components"": {
    ""Schemas"": {
      ""pullrequest"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""repository"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""user"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      }
    },
    ""Responses"": null,
    ""Parameters"": null,
    ""Examples"": null,
    ""RequestBodies"": null,
    ""Headers"": null,
    ""SecuritySchemes"": null,
    ""Links"": {
      ""PullRequestMerge"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Operation"": {
              ""Case"": ""Id"",
              ""Fields"": [
                ""mergePullRequest""
              ]
            },
            ""Parameters"": {
              ""pid"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/id""
                ]
              },
              ""slug"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/repository/slug""
                ]
              },
              ""username"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/author/username""
                ]
              }
            },
            ""RequestBody"": null,
            ""Description"": null,
            ""Server"": null
          }
        ]
      },
      ""RepositoryPullRequests"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Operation"": {
              ""Case"": ""Id"",
              ""Fields"": [
                ""getPullRequestsByRepository""
              ]
            },
            ""Parameters"": {
              ""slug"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/slug""
                ]
              },
              ""username"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/owner/username""
                ]
              }
            },
            ""RequestBody"": null,
            ""Description"": null,
            ""Server"": null
          }
        ]
      },
      ""UserRepositories"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Operation"": {
              ""Case"": ""Id"",
              ""Fields"": [
                ""getRepositoriesByOwner""
              ]
            },
            ""Parameters"": {
              ""username"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/username""
                ]
              }
            },
            ""RequestBody"": null,
            ""Description"": null,
            ""Server"": null
          }
        ]
      },
      ""UserRepository"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Operation"": {
              ""Case"": ""Id"",
              ""Fields"": [
                ""getRepository""
              ]
            },
            ""Parameters"": {
              ""slug"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/slug""
                ]
              },
              ""username"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  ""$response.body#/owner/username""
                ]
              }
            },
            ""RequestBody"": null,
            ""Description"": null,
            ""Server"": null
          }
        ]
      }
    },
    ""Callbacks"": null
  },
  ""Security"": null,
  ""Tags"": null,
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``Non-oauth scopes example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "non-oauth-scopes.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.1.0"",
  ""Info"": {
    ""Title"": ""Non-oAuth Scopes example"",
    ""Description"": null,
    ""TermsOfService"": null,
    ""Contact"": null,
    ""License"": null,
    ""Version"": ""1.0.0""
  },
  ""Servers"": null,
  ""Paths"": {
    ""Fields"": {
      ""/users"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": null,
          ""Parameters"": null,
          ""RequestBody"": null,
          ""Responses"": null,
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": [
            {
              ""Fields"": {
                ""bearerAuth"": [
                  ""read:users"",
                  ""public""
                ]
              }
            }
          ],
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      }
    }
  },
  ""Components"": {
    ""Schemas"": null,
    ""Responses"": null,
    ""Parameters"": null,
    ""Examples"": null,
    ""RequestBodies"": null,
    ""Headers"": null,
    ""SecuritySchemes"": {
      ""bearerAuth"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Case"": ""Http"",
            ""Fields"": [
              ""note: non-oauth scopes are not defined at the securityScheme level"",
              ""bearer"",
              ""jwt""
            ]
          }
        ]
      }
    },
    ""Links"": null,
    ""Callbacks"": null
  },
  ""Security"": null,
  ""Tags"": null,
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``Petstore example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "petstore.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.0.0"",
  ""Info"": {
    ""Title"": ""Swagger Petstore"",
    ""Description"": null,
    ""TermsOfService"": null,
    ""Contact"": null,
    ""License"": {
      ""Name"": ""MIT"",
      ""Url"": null
    },
    ""Version"": ""1.0.0""
  },
  ""Servers"": [
    {
      ""Url"": ""http://petstore.swagger.io/v1"",
      ""Description"": null,
      ""Variables"": null
    }
  ],
  ""Paths"": {
    ""Fields"": {
      ""/pets"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": [
            ""pets""
          ],
          ""Summary"": ""List all pets"",
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""listPets"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""limit"",
                  ""In"": {
                    ""Case"": ""Query""
                  },
                  ""Description"": ""How many items to return at one time (max 100)"",
                  ""Required"": false,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Description"": ""unexpected error"",
                  ""Headers"": null,
                  ""Content"": {
                    ""application/json"": {
                      ""Schema"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/schemas/Error""
                          }
                        ]
                      },
                      ""Example"": null,
                      ""Encoding"": null
                    }
                  },
                  ""Links"": null
                }
              ]
            },
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""A paged array of pets"",
                    ""Headers"": {
                      ""x-next"": {
                        ""Case"": ""Choice1Of2"",
                        ""Fields"": [
                          {
                            ""Description"": ""A link to the next page of responses"",
                            ""Required"": null,
                            ""Deprecated"": null,
                            ""AllowEmptyValue"": null,
                            ""Style"": null,
                            ""Explode"": null,
                            ""AllowReserved"": null,
                            ""Schema"": {
                              ""Case"": ""Choice1Of2"",
                              ""Fields"": [
                                null
                              ]
                            },
                            ""Example"": null,
                            ""Content"": null
                          }
                        ]
                      }
                    },
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/Pets""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": {
          ""Tags"": [
            ""pets""
          ],
          ""Summary"": ""Create a pet"",
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""createPets"",
          ""Parameters"": null,
          ""RequestBody"": {
            ""Case"": ""Choice1Of2"",
            ""Fields"": [
              {
                ""Description"": null,
                ""Content"": {
                  ""application/json"": {
                    ""Schema"": {
                      ""Case"": ""Choice2Of2"",
                      ""Fields"": [
                        {
                          ""Ref"": ""#/components/schemas/Pet""
                        }
                      ]
                    },
                    ""Example"": null,
                    ""Encoding"": null
                  }
                },
                ""Required"": true
              }
            ]
          },
          ""Responses"": {
            ""Default"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Description"": ""unexpected error"",
                  ""Headers"": null,
                  ""Content"": {
                    ""application/json"": {
                      ""Schema"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/schemas/Error""
                          }
                        ]
                      },
                      ""Example"": null,
                      ""Encoding"": null
                    }
                  },
                  ""Links"": null
                }
              ]
            },
            ""Patterns"": {
              ""201"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""Null response"",
                    ""Headers"": null,
                    ""Content"": null,
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/pets/{petId}"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": [
            ""pets""
          ],
          ""Summary"": ""Info for a specific pet"",
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""showPetById"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""petId"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": ""The id of the pet to retrieve"",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Description"": ""unexpected error"",
                  ""Headers"": null,
                  ""Content"": {
                    ""application/json"": {
                      ""Schema"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/schemas/Error""
                          }
                        ]
                      },
                      ""Example"": null,
                      ""Encoding"": null
                    }
                  },
                  ""Links"": null
                }
              ]
            },
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""Expected response to a valid request"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/Pet""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      }
    }
  },
  ""Components"": {
    ""Schemas"": {
      ""Error"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""Pet"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""Pets"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      }
    },
    ""Responses"": null,
    ""Parameters"": null,
    ""Examples"": null,
    ""RequestBodies"": null,
    ""Headers"": null,
    ""SecuritySchemes"": null,
    ""Links"": null,
    ""Callbacks"": null
  },
  ""Security"": null,
  ""Tags"": null,
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``Petstore expanded example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "petstore-expanded.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.0.0"",
  ""Info"": {
    ""Title"": ""Swagger Petstore"",
    ""Description"": ""A sample API that uses a petstore as an example to demonstrate features in the OpenAPI 3.0 specification"",
    ""TermsOfService"": ""http://swagger.io/terms/"",
    ""Contact"": {
      ""Name"": ""Swagger API Team"",
      ""Url"": ""http://swagger.io"",
      ""Email"": ""apiteam@swagger.io""
    },
    ""License"": {
      ""Name"": ""Apache 2.0"",
      ""Url"": ""https://www.apache.org/licenses/LICENSE-2.0.html""
    },
    ""Version"": ""1.0.0""
  },
  ""Servers"": [
    {
      ""Url"": ""https://petstore.swagger.io/v2"",
      ""Description"": null,
      ""Variables"": null
    }
  ],
  ""Paths"": {
    ""Fields"": {
      ""/pets"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": ""Returns all pets from the system that the user has access to\nNam sed condimentum est. Maecenas tempor sagittis sapien, nec rhoncus sem sagittis sit amet. Aenean at gravida augue, ac iaculis sem. Curabitur odio lorem, ornare eget elementum nec, cursus id lectus. Duis mi turpis, pulvinar ac eros ac, tincidunt varius justo. In hac habitasse platea dictumst. Integer at adipiscing ante, a sagittis ligula. Aenean pharetra tempor ante molestie imperdiet. Vivamus id aliquam diam. Cras quis velit non tortor eleifend sagittis. Praesent at enim pharetra urna volutpat venenatis eget eget mauris. In eleifend fermentum facilisis. Praesent enim enim, gravida ac sodales sed, placerat id erat. Suspendisse lacus dolor, consectetur non augue vel, vehicula interdum libero. Morbi euismod sagittis libero sed lacinia.\n\nSed tempus felis lobortis leo pulvinar rutrum. Nam mattis velit nisl, eu condimentum ligula luctus nec. Phasellus semper velit eget aliquet faucibus. In a mattis elit. Phasellus vel urna viverra, condimentum lorem id, rhoncus nibh. Ut pellentesque posuere elementum. Sed a varius odio. Morbi rhoncus ligula libero, vel eleifend nunc tristique vitae. Fusce et sem dui. Aenean nec scelerisque tortor. Fusce malesuada accumsan magna vel tempus. Quisque mollis felis eu dolor tristique, sit amet auctor felis gravida. Sed libero lorem, molestie sed nisl in, accumsan tempor nisi. Fusce sollicitudin massa ut lacinia mattis. Sed vel eleifend lorem. Pellentesque vitae felis pretium, pulvinar elit eu, euismod sapien.\n"",
          ""ExternalDocs"": null,
          ""OperationId"": ""findPets"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""tags"",
                  ""In"": {
                    ""Case"": ""Query""
                  },
                  ""Description"": ""tags to filter by"",
                  ""Required"": false,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": ""form"",
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""limit"",
                  ""In"": {
                    ""Case"": ""Query""
                  },
                  ""Description"": ""maximum number of results to return"",
                  ""Required"": false,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Description"": ""unexpected error"",
                  ""Headers"": null,
                  ""Content"": {
                    ""application/json"": {
                      ""Schema"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/schemas/Error""
                          }
                        ]
                      },
                      ""Example"": null,
                      ""Encoding"": null
                    }
                  },
                  ""Links"": null
                }
              ]
            },
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""pet response"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            null
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": ""Creates a new pet in the store. Duplicates are allowed"",
          ""ExternalDocs"": null,
          ""OperationId"": ""addPet"",
          ""Parameters"": null,
          ""RequestBody"": {
            ""Case"": ""Choice1Of2"",
            ""Fields"": [
              {
                ""Description"": ""Pet to add to the store"",
                ""Content"": {
                  ""application/json"": {
                    ""Schema"": {
                      ""Case"": ""Choice2Of2"",
                      ""Fields"": [
                        {
                          ""Ref"": ""#/components/schemas/NewPet""
                        }
                      ]
                    },
                    ""Example"": null,
                    ""Encoding"": null
                  }
                },
                ""Required"": true
              }
            ]
          },
          ""Responses"": {
            ""Default"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Description"": ""unexpected error"",
                  ""Headers"": null,
                  ""Content"": {
                    ""application/json"": {
                      ""Schema"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/schemas/Error""
                          }
                        ]
                      },
                      ""Example"": null,
                      ""Encoding"": null
                    }
                  },
                  ""Links"": null
                }
              ]
            },
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""pet response"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/Pet""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/pets/{id}"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": ""Returns a user based on a single ID, if the user does not have access to the pet"",
          ""ExternalDocs"": null,
          ""OperationId"": ""find pet by id"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""id"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": ""ID of pet to fetch"",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Description"": ""unexpected error"",
                  ""Headers"": null,
                  ""Content"": {
                    ""application/json"": {
                      ""Schema"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/schemas/Error""
                          }
                        ]
                      },
                      ""Example"": null,
                      ""Encoding"": null
                    }
                  },
                  ""Links"": null
                }
              ]
            },
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""pet response"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/Pet""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": {
          ""Tags"": null,
          ""Summary"": null,
          ""Description"": ""deletes a single pet based on the ID supplied"",
          ""ExternalDocs"": null,
          ""OperationId"": ""deletePet"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""id"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": ""ID of pet to delete"",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Description"": ""unexpected error"",
                  ""Headers"": null,
                  ""Content"": {
                    ""application/json"": {
                      ""Schema"": {
                        ""Case"": ""Choice2Of2"",
                        ""Fields"": [
                          {
                            ""Ref"": ""#/components/schemas/Error""
                          }
                        ]
                      },
                      ""Example"": null,
                      ""Encoding"": null
                    }
                  },
                  ""Links"": null
                }
              ]
            },
            ""Patterns"": {
              ""204"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""pet deleted"",
                    ""Headers"": null,
                    ""Content"": null,
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      }
    }
  },
  ""Components"": {
    ""Schemas"": {
      ""Error"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""NewPet"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""Pet"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      }
    },
    ""Responses"": null,
    ""Parameters"": null,
    ""Examples"": null,
    ""RequestBodies"": null,
    ""Headers"": null,
    ""SecuritySchemes"": null,
    ""Links"": null,
    ""Callbacks"": null
  },
  ""Security"": null,
  ""Tags"": null,
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``Tictactoe example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "tictactoe.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.1.0"",
  ""Info"": {
    ""Title"": ""Tic Tac Toe"",
    ""Description"": ""This API allows writing down marks on a Tic Tac Toe board\nand requesting the state of the board or of individual squares.\n"",
    ""TermsOfService"": null,
    ""Contact"": null,
    ""License"": null,
    ""Version"": ""1.0.0""
  },
  ""Servers"": null,
  ""Paths"": {
    ""Fields"": {
      ""/board"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": [
            ""Gameplay""
          ],
          ""Summary"": ""Get the whole board"",
          ""Description"": ""Retrieves the current state of the board and the winner."",
          ""ExternalDocs"": null,
          ""OperationId"": ""get-board"",
          ""Parameters"": null,
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""OK"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/status""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": [
            {
              ""Fields"": {
                ""defaultApiKey"": []
              }
            },
            {
              ""Fields"": {
                ""app2AppOauth"": [
                  ""board:read""
                ]
              }
            }
          ],
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/board/{row}/{column}"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": [
            ""Gameplay""
          ],
          ""Summary"": ""Get a single board square"",
          ""Description"": ""Retrieves the requested square."",
          ""ExternalDocs"": null,
          ""OperationId"": ""get-square"",
          ""Parameters"": null,
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""OK"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/mark""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              },
              ""400"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""The provided parameters are incorrect"",
                    ""Headers"": null,
                    ""Content"": {
                      ""text/html"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/errorMessage""
                            }
                          ]
                        },
                        ""Example"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            ""Illegal coordinates""
                          ]
                        },
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": [
            {
              ""Fields"": {
                ""bearerHttpAuthentication"": []
              }
            },
            {
              ""Fields"": {
                ""user2AppOauth"": [
                  ""board:read""
                ]
              }
            }
          ],
          ""Servers"": null
        },
        ""Put"": {
          ""Tags"": [
            ""Gameplay""
          ],
          ""Summary"": ""Set a single board square"",
          ""Description"": ""Places a mark on the board and retrieves the whole board and the winner (if any)."",
          ""ExternalDocs"": null,
          ""OperationId"": ""put-square"",
          ""Parameters"": null,
          ""RequestBody"": {
            ""Case"": ""Choice1Of2"",
            ""Fields"": [
              {
                ""Description"": null,
                ""Content"": {
                  ""application/json"": {
                    ""Schema"": {
                      ""Case"": ""Choice2Of2"",
                      ""Fields"": [
                        {
                          ""Ref"": ""#/components/schemas/mark""
                        }
                      ]
                    },
                    ""Example"": null,
                    ""Encoding"": null
                  }
                },
                ""Required"": true
              }
            ]
          },
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""OK"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/status""
                            }
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              },
              ""400"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""The provided parameters are incorrect"",
                    ""Headers"": null,
                    ""Content"": {
                      ""text/html"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/errorMessage""
                            }
                          ]
                        },
                        ""Example"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""illegalCoordinates"": {
                                ""Case"": ""Choice1Of2"",
                                ""Fields"": [
                                  {
                                    ""Summary"": null,
                                    ""Description"": null,
                                    ""Value"": {
                                      ""Case"": ""Choice1Of2"",
                                      ""Fields"": [
                                        ""Illegal coordinates.""
                                      ]
                                    }
                                  }
                                ]
                              },
                              ""invalidMark"": {
                                ""Case"": ""Choice1Of2"",
                                ""Fields"": [
                                  {
                                    ""Summary"": null,
                                    ""Description"": null,
                                    ""Value"": {
                                      ""Case"": ""Choice1Of2"",
                                      ""Fields"": [
                                        ""Invalid Mark (X or O).""
                                      ]
                                    }
                                  }
                                ]
                              },
                              ""notEmpty"": {
                                ""Case"": ""Choice1Of2"",
                                ""Fields"": [
                                  {
                                    ""Summary"": null,
                                    ""Description"": null,
                                    ""Value"": {
                                      ""Case"": ""Choice1Of2"",
                                      ""Fields"": [
                                        ""Square is not empty.""
                                      ]
                                    }
                                  }
                                ]
                              }
                            }
                          ]
                        },
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": [
            {
              ""Fields"": {
                ""bearerHttpAuthentication"": []
              }
            },
            {
              ""Fields"": {
                ""user2AppOauth"": [
                  ""board:write""
                ]
              }
            }
          ],
          ""Servers"": null
        },
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": [
          {
            ""Case"": ""Choice2Of2"",
            ""Fields"": [
              {
                ""Ref"": ""#/components/parameters/rowParam""
              }
            ]
          },
          {
            ""Case"": ""Choice2Of2"",
            ""Fields"": [
              {
                ""Ref"": ""#/components/parameters/columnParam""
              }
            ]
          }
        ]
      }
    }
  },
  ""Components"": {
    ""Schemas"": {
      ""board"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""coordinate"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""errorMessage"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""mark"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""status"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      },
      ""winner"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      }
    },
    ""Responses"": null,
    ""Parameters"": {
      ""columnParam"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Name"": ""column"",
            ""In"": {
              ""Case"": ""Path""
            },
            ""Description"": ""Board column (horizontal coordinate)"",
            ""Required"": true,
            ""Deprecated"": null,
            ""AllowEmptyValue"": null,
            ""Style"": null,
            ""Explode"": null,
            ""AllowReserved"": null,
            ""Schema"": {
              ""Case"": ""Choice2Of2"",
              ""Fields"": [
                {
                  ""Ref"": ""#/components/schemas/coordinate""
                }
              ]
            },
            ""Example"": null,
            ""Content"": null
          }
        ]
      },
      ""rowParam"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Name"": ""row"",
            ""In"": {
              ""Case"": ""Path""
            },
            ""Description"": ""Board row (vertical coordinate)"",
            ""Required"": true,
            ""Deprecated"": null,
            ""AllowEmptyValue"": null,
            ""Style"": null,
            ""Explode"": null,
            ""AllowReserved"": null,
            ""Schema"": {
              ""Case"": ""Choice2Of2"",
              ""Fields"": [
                {
                  ""Ref"": ""#/components/schemas/coordinate""
                }
              ]
            },
            ""Example"": null,
            ""Content"": null
          }
        ]
      }
    },
    ""Examples"": null,
    ""RequestBodies"": null,
    ""Headers"": null,
    ""SecuritySchemes"": {
      ""app2AppOauth"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Case"": ""Oauth2"",
            ""Fields"": [
              null,
              {
                ""Implicit"": null,
                ""Password"": null,
                ""ClientCredentials"": {
                  ""AuthorizationUrl"": null,
                  ""TokenUrl"": ""https://learn.openapis.org/oauth/2.0/token"",
                  ""RefreshUrl"": null,
                  ""Scopes"": {
                    ""board:read"": ""Read the board""
                  }
                },
                ""AuthorizationCode"": null
              }
            ]
          }
        ]
      },
      ""basicHttpAuthentication"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Case"": ""Http"",
            ""Fields"": [
              ""Basic HTTP Authentication"",
              ""Basic"",
              null
            ]
          }
        ]
      },
      ""bearerHttpAuthentication"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Case"": ""Http"",
            ""Fields"": [
              ""Bearer token using a JWT"",
              ""Bearer"",
              ""JWT""
            ]
          }
        ]
      },
      ""defaultApiKey"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Case"": ""ApiKey"",
            ""Fields"": [
              ""API key provided in console"",
              ""api-key"",
              {
                ""Case"": ""Header""
              }
            ]
          }
        ]
      },
      ""user2AppOauth"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          {
            ""Case"": ""Oauth2"",
            ""Fields"": [
              null,
              {
                ""Implicit"": null,
                ""Password"": null,
                ""ClientCredentials"": null,
                ""AuthorizationCode"": {
                  ""AuthorizationUrl"": ""https://learn.openapis.org/oauth/2.0/auth"",
                  ""TokenUrl"": ""https://learn.openapis.org/oauth/2.0/token"",
                  ""RefreshUrl"": null,
                  ""Scopes"": {
                    ""board:read"": ""Read the board"",
                    ""board:write"": ""Write to the board""
                  }
                }
              }
            ]
          }
        ]
      }
    },
    ""Links"": null,
    ""Callbacks"": null
  },
  ""Security"": null,
  ""Tags"": [
    {
      ""Name"": ""Gameplay"",
      ""Description"": null,
      ""ExternalDocs"": null
    }
  ],
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``uspto example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "uspto.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.0.1"",
  ""Info"": {
    ""Title"": ""USPTO Data Set API"",
    ""Description"": ""The Data Set API (DSAPI) allows the public users to discover and search USPTO exported data sets. This is a generic API that allows USPTO users to make any CSV based data files searchable through API. With the help of GET call, it returns the list of data fields that are searchable. With the help of POST call, data can be fetched based on the filters on the field names. Please note that POST call is used to search the actual data. The reason for the POST call is that it allows users to specify any complex search criteria without worry about the GET size limitations as well as encoding of the input parameters."",
    ""TermsOfService"": null,
    ""Contact"": {
      ""Name"": ""Open Data Portal"",
      ""Url"": ""https://developer.uspto.gov"",
      ""Email"": ""developer@uspto.gov""
    },
    ""License"": null,
    ""Version"": ""1.0.0""
  },
  ""Servers"": [
    {
      ""Url"": ""{scheme}://developer.uspto.gov/ds-api"",
      ""Description"": null,
      ""Variables"": {
        ""scheme"": {
          ""Enum"": [
            ""https"",
            ""http""
          ],
          ""Default"": ""https"",
          ""Description"": ""The Data Set API is accessible via https and http""
        }
      }
    }
  ],
  ""Paths"": {
    ""Fields"": {
      ""/"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": [
            ""metadata""
          ],
          ""Summary"": ""List available data sets"",
          ""Description"": null,
          ""ExternalDocs"": null,
          ""OperationId"": ""list-data-sets"",
          ""Parameters"": null,
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""Returns a list of data sets"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice2Of2"",
                          ""Fields"": [
                            {
                              ""Ref"": ""#/components/schemas/dataSetList""
                            }
                          ]
                        },
                        ""Example"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            {
                              ""total"": 2,
                              ""apis"": [
                                {
                                  ""apiKey"": ""oa_citations"",
                                  ""apiVersionNumber"": ""v1"",
                                  ""apiUrl"": ""https://developer.uspto.gov/ds-api/oa_citations/v1/fields"",
                                  ""apiDocumentationUrl"": ""https://developer.uspto.gov/ds-api-docs/index.html?url=https://developer.uspto.gov/ds-api/swagger/docs/oa_citations.json""
                                },
                                {
                                  ""apiKey"": ""cancer_moonshot"",
                                  ""apiVersionNumber"": ""v1"",
                                  ""apiUrl"": ""https://developer.uspto.gov/ds-api/cancer_moonshot/v1/fields"",
                                  ""apiDocumentationUrl"": ""https://developer.uspto.gov/ds-api-docs/index.html?url=https://developer.uspto.gov/ds-api/swagger/docs/cancer_moonshot.json""
                                }
                              ]
                            }
                          ]
                        },
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/{dataset}/{version}/fields"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": {
          ""Tags"": [
            ""metadata""
          ],
          ""Summary"": ""Provides the general information about the API and the list of fields that can be used to query the dataset."",
          ""Description"": ""This GET API returns the list of all the searchable field names that are in the oa_citations. Please see the \u0027fields\u0027 attribute which returns an array of field names. Each field or a combination of fields can be searched using the syntax options shown below."",
          ""ExternalDocs"": null,
          ""OperationId"": ""list-searchable-fields"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""dataset"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": ""Name of the dataset."",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      ""oa_citations""
                    ]
                  },
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""version"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": ""Version of the dataset."",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      ""v1""
                    ]
                  },
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": null,
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""The dataset API for the given version is found and it is accessible to consume."",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            null
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              },
              ""404"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""The combination of dataset name and version is not found in the system or it is not published yet to be consumed by public."",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            null
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Put"": null,
        ""Post"": null,
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      },
      ""/{dataset}/{version}/records"": {
        ""Ref"": null,
        ""Summary"": null,
        ""Description"": null,
        ""Get"": null,
        ""Put"": null,
        ""Post"": {
          ""Tags"": [
            ""search""
          ],
          ""Summary"": ""Provides search capability for the data set with the given search criteria."",
          ""Description"": ""This API is based on Solr/Lucene Search. The data is indexed using SOLR. This GET API returns the list of all the searchable field names that are in the Solr Index. Please see the \u0027fields\u0027 attribute which returns an array of field names. Each field or a combination of fields can be searched using the Solr/Lucene Syntax. Please refer https://lucene.apache.org/core/3_6_2/queryparsersyntax.html#Overview for the query syntax. List of field names that are searchable can be determined using above GET api."",
          ""ExternalDocs"": null,
          ""OperationId"": ""perform-search"",
          ""Parameters"": [
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""version"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": ""Version of the dataset."",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            },
            {
              ""Case"": ""Choice1Of2"",
              ""Fields"": [
                {
                  ""Name"": ""dataset"",
                  ""In"": {
                    ""Case"": ""Path""
                  },
                  ""Description"": ""Name of the dataset. In this case, the default value is oa_citations"",
                  ""Required"": true,
                  ""Deprecated"": null,
                  ""AllowEmptyValue"": null,
                  ""Style"": null,
                  ""Explode"": null,
                  ""AllowReserved"": null,
                  ""Schema"": {
                    ""Case"": ""Choice1Of2"",
                    ""Fields"": [
                      null
                    ]
                  },
                  ""Example"": null,
                  ""Content"": null
                }
              ]
            }
          ],
          ""RequestBody"": {
            ""Case"": ""Choice1Of2"",
            ""Fields"": [
              {
                ""Description"": null,
                ""Content"": {
                  ""application/x-www-form-urlencoded"": {
                    ""Schema"": {
                      ""Case"": ""Choice1Of2"",
                      ""Fields"": [
                        null
                      ]
                    },
                    ""Example"": null,
                    ""Encoding"": null
                  }
                },
                ""Required"": null
              }
            ]
          },
          ""Responses"": {
            ""Default"": null,
            ""Patterns"": {
              ""200"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""successful operation"",
                    ""Headers"": null,
                    ""Content"": {
                      ""application/json"": {
                        ""Schema"": {
                          ""Case"": ""Choice1Of2"",
                          ""Fields"": [
                            null
                          ]
                        },
                        ""Example"": null,
                        ""Encoding"": null
                      }
                    },
                    ""Links"": null
                  }
                ]
              },
              ""404"": {
                ""Case"": ""Choice1Of2"",
                ""Fields"": [
                  {
                    ""Description"": ""No matching record found for the given criteria."",
                    ""Headers"": null,
                    ""Content"": null,
                    ""Links"": null
                  }
                ]
              }
            }
          },
          ""Callbacks"": null,
          ""Deprecated"": null,
          ""Security"": null,
          ""Servers"": null
        },
        ""Delete"": null,
        ""Options"": null,
        ""Head"": null,
        ""Patch"": null,
        ""Trace"": null,
        ""Servers"": null,
        ""Parameters"": null
      }
    }
  },
  ""Components"": {
    ""Schemas"": {
      ""dataSetList"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      }
    },
    ""Responses"": null,
    ""Parameters"": null,
    ""Examples"": null,
    ""RequestBodies"": null,
    ""Headers"": null,
    ""SecuritySchemes"": null,
    ""Links"": null,
    ""Callbacks"": null
  },
  ""Security"": null,
  ""Tags"": [
    {
      ""Name"": ""metadata"",
      ""Description"": ""Find out about the data sets"",
      ""ExternalDocs"": null
    },
    {
      ""Name"": ""search"",
      ""Description"": ""Search a data set"",
      ""ExternalDocs"": null
    }
  ],
  ""ExternalDocs"": null
}"

            return actual
        }

    [<Test>]
    let ``webhook example`` () =
        let resource =
            Assembly.getEmbeddedResource typeof<Dummy>.Assembly "webhook-example.json"
            |> JsonNode.Parse
            |> _.AsObject()

        let actual = OpenApiSpec.Parse resource

        expect {
            snapshotJson
                @"{
  ""OpenApi"": ""3.1.0"",
  ""Info"": {
    ""Title"": ""Webhook Example"",
    ""Description"": null,
    ""TermsOfService"": null,
    ""Contact"": null,
    ""License"": null,
    ""Version"": ""1.0.0""
  },
  ""Servers"": null,
  ""Paths"": null,
  ""Components"": {
    ""Schemas"": {
      ""Pet"": {
        ""Case"": ""Choice1Of2"",
        ""Fields"": [
          null
        ]
      }
    },
    ""Responses"": null,
    ""Parameters"": null,
    ""Examples"": null,
    ""RequestBodies"": null,
    ""Headers"": null,
    ""SecuritySchemes"": null,
    ""Links"": null,
    ""Callbacks"": null
  },
  ""Security"": null,
  ""Tags"": null,
  ""ExternalDocs"": null
}"

            return actual
        }
