set -eux
thisDir=$(dirname "$0")
tempDir=$thisDir/../temp

offset=${1:-500000}

nowSeconds=$(date +%s)
now=$(($nowSeconds*1000))
beforeTime=$((now - (100*1000)))
timestamp=$(($nowSeconds*1000+$offset))
endValidTime=$(($timestamp - 60000))
batcherOffset=120
batcherEndTime=$(($batcherOffset*1000+$timestamp))
prefix=${2:-0}

mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix
mkdir -p $tempDir/$BLOCKCHAIN_PREFIX/redeemers

sellerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/seller-pkh.txt)
marketplacePkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/marketplace-pkh.txt)
royaltyPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/royalities-pkh.txt)
buyerPkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/buyer-pkh.txt)
buyer1Pkh=$(cat $tempDir/$BLOCKCHAIN_PREFIX/pkhs/buyer1-pkh.txt)
batcherValidatorHash=$(cat $thisDir/auction-hash.txt)
escrowValidatorHash=$(cat $thisDir/escrow-hash.txt)
bidMinterHash=$(cat $thisDir/bid-minter-hash.txt)
activityPolicyId=$(cat $thisDir/activity-minter-hash.txt)
activityTokenName=494E444558

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/start.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": $batcherEndTime
    },
    {
      "int": 8000000
    },
    {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 1,
      "fields": [
      ]
    },
    {
      "bytes": "$escrowValidatorHash"
    },
    {
      "map": [
        {
          "k": {
              "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
            },
          "v": {
            "map":[
              { "k":
                {
                  "bytes": "123456"
                },
                "v":
                {
                  "int": 1
                }
              }
            ]
          }
        }
      ]
    },
    {
      "bytes": "$bidMinterHash"
    },
    {
      "bytes": "4143544956495459"
    },
    {
      "bytes": "$activityPolicyId"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-1.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": $batcherEndTime
    },
    {
      "int": 8000000
    },
        {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$buyerPkh"
            },
            {
              "int" : 10000000
            },
            {
              "int": $endValidTime
            }
          ]
        }
      ]
    },
    {
      "bytes": "$escrowValidatorHash"
    },
    {
      "map": [
        {
          "k": {
              "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
            },
          "v": {
            "map":[
              { "k":
                {
                  "bytes": "123456"
                },
                "v":
                {
                  "int": 1
                }
              }
            ]
          }
        }
      ]
    },
    {
      "bytes": "$bidMinterHash"
    },
    {
      "bytes": "4143544956495459"
    },
    {
      "bytes": "$activityPolicyId"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/reserve-not-met-bid-1.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": $batcherEndTime
    },
    {
      "int": 8000000
    },
        {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$buyerPkh"
            },
            {
              "int" : 7000000
            },
            {
              "int": $endValidTime
            }
          ]
        }
      ]
    },
    {
      "bytes": "$escrowValidatorHash"
    },
    {
      "map": [
        {
          "k": {
              "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
            },
          "v": {
            "map":[
              { "k":
                {
                  "bytes": "123456"
                },
                "v":
                {
                  "int": 1
                }
              }
            ]
          }
        }
      ]
    },
    {
      "bytes": "$bidMinterHash"
    },
    {
      "bytes": "4143544956495459"
    },
    {
      "bytes": "$activityPolicyId"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/seller-bid-1.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": $batcherEndTime
    },
    {
      "int": 8000000
    },
    {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$sellerPkh"
            },
            {
              "int" : 10000000
            },
            {
              "int": $endValidTime
            }
          ]
        }
      ]
    },
    {
      "bytes": "$escrowValidatorHash"
    },
    {
      "map": [
        {
          "k": {
              "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
            },
          "v": {
            "map":[
              { "k":
                {
                  "bytes": "123456"
                },
                "v":
                {
                  "int": 1
                }
              }
            ]
          }
        }
      ]
    },
    {
      "bytes": "$bidMinterHash"
    },
    {
      "bytes": "4143544956495459"
    },
    {
      "bytes": "$activityPolicyId"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-2.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": $batcherEndTime
    },
    {
      "int": 8000000
    },
    {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$buyer1Pkh"
            },
            {
              "int" : 30000000
            },
            {
              "int": $endValidTime
            }
          ]
        }
      ]
    },
    {
      "bytes": "$escrowValidatorHash"
    },
    {
      "map": [
        {
          "k": {
              "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
            },
          "v": {
            "map":[
              { "k":
                {
                  "bytes": "123456"
                },
                "v":
                {
                  "int": 1
                }
              }
            ]
          }
        }
      ]
    },
    {
      "bytes": "$bidMinterHash"
    },
    {
      "bytes": "4143544956495459"
    },
    {
      "bytes": "$activityPolicyId"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/escrow-bid-1.json

{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$buyerPkh"
    },
    {
      "constructor": 0,
      "fields": [
        {
          "int": 10000000
        },
        {
          "map": [
            {
              "k": {
                  "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
                },
              "v": {
                "map":[
                  { "k":
                    {
                      "bytes": "123456"
                    },
                    "v":
                    {
                      "int": 1
                    }
                  }
                ]
              }
            }
          ]
        },
        {
          "int": $beforeTime
        },
        {
          "int": $endValidTime
        }
      ]
    },
    {
      "bytes": "$batcherValidatorHash"
    },
    {
      "bytes": "$bidMinterHash"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/escrow-reserve-not-met.json

{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$buyerPkh"
    },
    {
      "constructor": 0,
      "fields": [
        {
          "int": 7000000
        },
        {
          "map": [
            {
              "k": {
                  "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
                },
              "v": {
                "map":[
                  { "k":
                    {
                      "bytes": "123456"
                    },
                    "v":
                    {
                      "int": 1
                    }
                  }
                ]
              }
            }
          ]
        },
        {
          "int": $beforeTime
        },
        {
          "int": $endValidTime
        }
      ]
    },
    {
      "bytes": "$batcherValidatorHash"
    },
    {
      "bytes": "$bidMinterHash"
    }
  ]
}

EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/escrow-bid-2.json


{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$buyer1Pkh"
    },
    {
      "constructor": 0,
      "fields": [
        {
          "int": 30000000
        },
        {
          "map": [
            {
              "k": {
                  "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
                },
              "v": {
                "map":[
                  { "k":
                    {
                      "bytes": "123456"
                    },
                    "v":
                    {
                      "int": 1
                    }
                  }
                ]
              }
            }
          ]
        },
        {
          "int": $beforeTime
        },
        {
          "int": $endValidTime
        }
      ]
    },
    {
      "bytes": "$batcherValidatorHash"
    },
    {
      "bytes": "$bidMinterHash"
    }
  ]
}

EOF


cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/escrow-bid-3.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "constructor": 0,
      "fields": [
        {
          "int": 10000000
        },
        {
          "map": [
            {
              "k": {
                  "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
                },
              "v": {
                "map":[
                  { "k":
                    {
                      "bytes": "123456"
                    },
                    "v":
                    {
                      "int": 1
                    }
                  }
                ]
              }
            }
          ]
        },
        {
          "int": $beforeTime
        },
        {
          "int": $endValidTime
        }
      ]
    },
    {
      "bytes": "$batcherValidatorHash"
    },
    {
      "bytes": "$bidMinterHash"
    }
  ]
}
EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/sellerExchange.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$sellerPkh"
        }
      ]
    }
  ]
}
EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/buyerExchange.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$buyerPkh"
        }
      ]
    }
  ]
}
EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/buyer1Exchange.json
{
  "constructor": 0,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "bytes": "$buyer1Pkh"
        }
      ]
    }
  ]
}
EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/index0.json
{
  "constructor": 1,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "int": 0
        },
        {
          "bytes": "$activityPolicyId"
        },
        {
          "bytes": "$activityTokenName"
        }
      ]
    }
  ]
}
EOF

cat << EOF > $tempDir/$BLOCKCHAIN_PREFIX/datums/$prefix/index1.json
{
  "constructor": 1,
  "fields": [
    {
      "constructor": 0,
      "fields": [
        {
          "int": 1
        },
        {
          "bytes": "$activityPolicyId"
        },
        {
          "bytes": "$activityTokenName"
        }
      ]
    }
  ]
}
EOF

$thisDir/hash-datums.sh
