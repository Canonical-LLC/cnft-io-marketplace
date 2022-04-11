set -eu
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

offset=${1:-500000}

nowSeconds=$(date +%s)
now=$(($nowSeconds*1000))
timestamp=$(($nowSeconds*1000+$offset))
betterOfferTimestamp=$(($timestamp+5000000000))
prefix=${2:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix

sellerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/seller-pkh.txt)
marketplacePkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/marketplace-pkh.txt)
royaltyPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/royalities-pkh.txt)
buyerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/buyer-pkh.txt)
activityPolicyId=$(cat $thisDir/activity-minter-hash.txt)
activityTokenName=4143544956495459
boostTokenName=434E4654494F
boostPolicyId=$(cat $thisDir/test-policies/test-policy-0-id.txt)


cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/buy.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "list": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$sellerPkh"
            },
            {
              "map": [
                {
                  "k": {
                      "bytes": ""
                    },
                  "v": {
                    "map":[
                      { "k":
                        {
                          "bytes": ""
                        },
                        "v":
                        {
                          "int": 8000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$marketplacePkh"
            },
            {
              "map": [
                {
                  "k":
                    {
                      "bytes": ""
                    },
                  "v": {
                    "map": [
                      { "k":
                        {
                          "bytes": ""
                        }
                      ,
                        "v":
                        {
                          "int": 1000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$royaltyPkh"
            },
            {
              "map": [
                {
                  "k": {
                      "bytes": ""
                    },
                  "v": {
                    "map": [
                      {
                        "k": {
                      "bytes": ""
                    },
                        "v":
                        {
                          "int": 1000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        }
      ]
    },
    { "constructor": 1,
      "fields": []
    },
    {
      "bytes": "$activityTokenName"
    },
    {
      "bytes": "$activityPolicyId"
    },
    {
      "bytes": "$boostTokenName"
    },
    {
      "bytes": "$boostPolicyId"
    },
    {
      "bytes": "$marketplacePkh"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix/buy.json
{
  "constructor": 1,
  "fields": [
    {
      "list": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$buyerPkh"
            },
            {
              "map": [
                {
                  "k": {
                    "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
                  },
                  "v": {
                    "map": [
                      {
                        "k": {
                          "bytes": "123456"
                        },
                        "v": {
                          "int": 1
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/buy2.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "list": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$sellerPkh"
            },
            {
              "map": [
                {
                  "k": {
                      "bytes": ""
                    },
                  "v": {
                    "map":[
                      { "k":
                        {
                          "bytes": ""
                        },
                        "v":
                        {
                          "int": 8000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$marketplacePkh"
            },
            {
              "map": [
                {
                  "k":
                    {
                      "bytes": ""
                    },
                  "v": {
                    "map": [
                      { "k":
                        {
                          "bytes": ""
                        }
                      ,
                        "v":
                        {
                          "int": 1000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$royaltyPkh"
            },
            {
              "map": [
                {
                  "k": {
                      "bytes": ""
                    },
                  "v": {
                    "map": [
                      {
                        "k": {
                      "bytes": ""
                    },
                        "v":
                        {
                          "int": 1000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        }
      ]
    },
    { "constructor": 1,
      "fields": []
    },
    {
      "bytes": "$activityTokenName"
    },
    {
      "bytes": "$activityPolicyId"
    },
    {
      "bytes": "$boostTokenName"
    },
    {
      "bytes": "$boostPolicyId"
    },
    {
      "bytes": "$marketplacePkh"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix/buy2.json
{
  "constructor": 1,
  "fields": [
    {
      "list": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$buyerPkh"
            },
            {
              "map": [
                {
                  "k": {
                    "bytes": "380eab015ac8e52853df3ac291f0511b8a1b7d9ee77248917eaeef10"
                  },
                  "v": {
                    "map": [
                      {
                        "k": {
                          "bytes": "123456"
                        },
                        "v": {
                          "int": 1
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/offer.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$buyerPkh"
    },
    {
      "list": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$buyerPkh"
            },
            {
              "map": [
                {
                  "k": {
                    "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
                  },
                  "v": {
                    "map": [
                      {
                        "k": {
                          "bytes": "123456"
                        },
                        "v": {
                          "int": 1
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$marketplacePkh"
            },
            {
              "map": [
                {
                  "k":
                    {
                      "bytes": ""
                    },
                  "v": {
                    "map": [
                      { "k":
                        {
                          "bytes": ""
                        }
                      ,
                        "v":
                        {
                          "int": 1000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        },
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$royaltyPkh"
            },
            {
              "map": [
                {
                  "k": {
                      "bytes": ""
                    },
                  "v": {
                    "map": [
                      {
                        "k": {
                      "bytes": ""
                    },
                        "v":
                        {
                          "int": 1000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields" : [
            {
              "constructor": 0,
              "fields" : [
                {
                  "int": $timestamp
                }
              ]
            },
            {
              "constructor": 1,
              "fields": []
            },
            {
              "map": [
                {
                  "k": {
                      "bytes": ""
                    },
                  "v": {
                    "map": [
                      {
                        "k": {
                      "bytes": ""
                    },
                        "v":
                        {
                          "int": 10000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "bytes": "$activityTokenName"
    },
    {
      "bytes": "$activityPolicyId"
    },
    {
      "bytes": "$boostTokenName"
    },
    {
      "bytes": "$boostPolicyId"
    },
    {
      "bytes": "$marketplacePkh"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/redeemers/$prefix/offer.json
{
  "constructor": 1,
  "fields": [
    {
      "list": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes": "$sellerPkh"
            },
            {
              "map": [
                {
                  "k": {
                      "bytes": ""
                    },
                  "v": {
                    "map":[
                      { "k":
                        {
                          "bytes": ""
                        },
                        "v":
                        {
                          "int": 8000000
                        }
                      }
                    ]
                  }
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}

EOF
