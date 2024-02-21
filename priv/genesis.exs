alias Ippan.{Validator, Token, Account}

pk =
  <<133, 210, 110, 113, 239, 43, 61, 189, 153, 31, 241, 205, 62, 28, 241, 50, 184, 225, 166, 252,
    172, 96, 246, 11, 32, 130, 167, 194, 57, 206, 148, 104>>

npk =
  <<57, 226, 122, 248, 119, 36, 34, 255, 67, 171, 64, 214, 205, 139, 81, 36, 52, 62, 5, 16, 4,
    145, 130, 250, 232, 120, 95, 149, 222, 91, 123, 15, 74, 25, 106, 159, 15, 148, 141, 85, 214,
    218, 182, 9, 238, 215, 59, 103, 159, 195, 185, 68, 145, 191, 105, 224, 252, 67, 185, 97, 85,
    181, 118, 212, 217, 112, 101, 249, 73, 253, 33, 84, 62, 116, 20, 178, 230, 29, 224, 81, 169,
    45, 48, 132, 204, 25, 99, 64, 200, 39, 41, 12, 189, 124, 165, 72, 97, 141, 114, 0, 50, 163,
    252, 141, 29, 116, 155, 51, 194, 207, 189, 151, 93, 57, 213, 253, 105, 122, 120, 71, 185, 157,
    210, 200, 79, 154, 169, 20, 139, 183, 254, 244, 88, 138, 245, 244, 154, 149, 220, 232, 104,
    214, 25, 241, 163, 31, 44, 80, 22, 118, 182, 55, 84, 18, 54, 200, 153, 149, 199, 53, 34, 35,
    154, 216, 99, 189, 40, 245, 143, 240, 223, 252, 209, 36, 214, 69, 113, 196, 177, 221, 131, 84,
    192, 34, 174, 163, 61, 198, 75, 164, 19, 101, 40, 127, 36, 28, 94, 155, 93, 206, 245, 37, 42,
    217, 76, 87, 54, 43, 233, 192, 185, 186, 104, 151, 59, 57, 167, 137, 56, 227, 19, 241, 239,
    152, 112, 190, 69, 180, 64, 44, 19, 100, 149, 245, 219, 44, 125, 42, 192, 163, 15, 232, 67,
    187, 250, 156, 49, 224, 7, 154, 191, 226, 98, 12, 171, 93, 93, 31, 32, 255, 58, 239, 86, 226,
    165, 194, 232, 218, 13, 53, 50, 97, 197, 11, 88, 5, 79, 164, 20, 173, 48, 42, 28, 240, 157,
    51, 224, 145, 249, 74, 182, 54, 160, 119, 73, 199, 168, 108, 58, 96, 194, 217, 13, 167, 203,
    95, 114, 200, 221, 105, 16, 8, 171, 214, 58, 197, 157, 89, 66, 146, 80, 17, 233, 227, 112, 1,
    68, 105, 169, 132, 247, 139, 126, 63, 113, 222, 120, 133, 115, 167, 228, 83, 252, 180, 29,
    228, 148, 46, 102, 100, 40, 185, 146, 197, 255, 123, 192, 233, 165, 148, 15, 106, 225, 149,
    19, 232, 110, 181, 105, 79, 172, 204, 91, 212, 17, 182, 190, 217, 22, 31, 89, 67, 2, 41, 72,
    20, 94, 233, 48, 122, 37, 130, 116, 3, 109, 110, 127, 134, 103, 146, 68, 157, 142, 49, 71,
    136, 65, 26, 105, 117, 163, 216, 29, 130, 73, 90, 135, 104, 226, 84, 110, 207, 108, 237, 180,
    251, 130, 68, 200, 79, 161, 93, 151, 253, 143, 87, 164, 135, 89, 43, 200, 116, 3, 98, 255, 0,
    218, 200, 192, 228, 108, 251, 69, 78, 190, 166, 55, 103, 66, 16, 224, 240, 204, 141, 178, 36,
    129, 212, 78, 61, 161, 0, 28, 202, 125, 16, 175, 49, 5, 81, 78, 250, 236, 77, 244, 184, 13,
    128, 134, 205, 40, 130, 103, 131, 79, 92, 97, 129, 184, 180, 43, 252, 39, 12, 162, 237, 35,
    25, 220, 227, 81, 98, 204, 107, 82, 94, 11, 212, 222, 1, 13, 98, 81, 241, 188, 8, 87, 134,
    241, 220, 178, 109, 246, 182, 210, 18, 147, 43, 103, 184, 233, 223, 186, 170, 208, 169, 52,
    203, 225, 254, 174, 216, 6, 216, 105, 90, 61, 219, 112, 240, 249, 86, 10, 99, 217, 121, 55,
    137, 207, 33, 185, 224, 201, 183, 45, 8, 235, 31, 208, 180, 98, 4, 49, 46, 163, 231, 13, 55,
    3, 255, 22, 24, 217, 115, 58, 92, 222, 218, 184, 175, 225, 115, 129, 159, 30, 4, 24, 199, 248,
    104, 107, 172, 66, 215, 36, 183, 80, 246, 236, 224, 154, 141, 91, 201, 114, 154, 98, 114, 58,
    228, 52, 231, 210, 120, 202, 193, 100, 134, 65, 11, 118, 193, 28, 12, 233, 178, 233, 215, 194,
    106, 25, 7, 20, 31, 78, 153, 205, 9, 36, 94, 60, 209, 56, 112, 90, 142, 211, 232, 54, 235, 56,
    18, 27, 8, 66, 152, 231, 140, 32, 4, 132, 237, 186, 74, 113, 205, 113, 127, 235, 7, 111, 64,
    182, 153, 19, 128, 236, 185, 148, 121, 79, 232, 183, 51, 26, 139, 138, 35, 198, 237, 101, 55,
    255, 140, 104, 220, 209, 172, 50, 69, 163, 38, 116, 35, 176, 212, 147, 161, 176, 72, 8, 146,
    233, 38, 227, 167, 125, 255, 11, 18, 246, 70, 217, 56, 33, 242, 175, 35, 10, 101, 221, 201,
    172, 106, 219, 66, 79, 70, 248, 50, 74, 150, 206, 26, 189, 147, 142, 234, 164, 175, 186, 212,
    54, 69, 23, 236, 22, 195, 9, 57, 143, 44, 215, 97, 82, 130, 241, 190, 37, 182, 38, 160, 110,
    11, 235, 226, 180, 63, 204, 137, 230, 247, 201, 61, 182, 1, 74, 34, 32, 96, 78, 61, 226, 8,
    30, 155, 223, 18, 59, 123, 182, 159, 254, 194, 177, 176, 10, 13, 50, 79, 160, 242, 117, 161,
    8, 164, 234, 59, 143, 148, 229, 107, 187, 101, 13, 152, 170, 87, 122, 88, 216, 130, 77, 124,
    174, 236, 77, 5, 223, 40, 2, 72, 247, 176, 235, 83, 139, 202, 142, 213, 85, 12, 80, 129, 91,
    23, 3, 121, 111, 98, 46, 220, 35, 80, 221, 89, 255, 19, 173, 154, 210, 230, 124, 186, 191,
    133, 143, 53, 167, 188, 10, 223, 168, 146, 59, 146, 73, 95, 251, 122, 69, 22, 175, 134, 14,
    127, 65, 133, 87, 25, 252, 194, 219, 179, 247, 250, 39, 156, 182, 76, 152, 79, 80, 216, 203,
    14, 5, 123, 211, 208, 96, 11, 114, 233, 202, 136, 130, 170, 227, 146, 185, 0, 178, 70, 245,
    122, 124, 108, 214, 212, 47, 141, 8, 84, 239, 22, 216, 65, 153, 150, 134, 160, 121, 149, 75,
    250, 133, 220, 80, 165, 67, 129, 0, 94, 69, 138, 36, 238, 150, 214, 135, 73, 48, 252, 220,
    149, 108, 177, 56, 159, 214, 164, 135, 23, 122, 49, 245, 160, 127, 142, 116, 11, 227, 233,
    220, 195, 118, 247, 236, 43, 212, 147, 107, 197, 114, 234, 43, 230, 182, 150, 237, 6, 25, 107,
    17, 146, 230, 14, 66, 197, 231, 174, 177, 152, 230, 88, 136, 219, 106, 113, 48, 223, 8, 154,
    63, 20, 115, 89, 5, 88, 45, 224, 223, 54, 50, 215, 86, 111, 134, 200, 193, 60, 24, 38, 225,
    171, 158, 93, 37, 204, 173, 119, 94, 181, 193, 163, 48, 178, 210, 200, 202, 27, 212, 227, 36,
    97, 68, 88, 96, 41, 28, 39, 109, 69, 52, 228, 12, 46, 102, 10, 194, 23, 12, 150, 46, 243, 127,
    185, 186, 144, 79, 233, 251, 32, 6, 138, 219, 74, 18, 65, 222, 71, 228, 32, 0, 97, 92, 3>>

address = "0x2Qyubpv2bgy8bDZ7UpkuwabHjJdc"

%{
  "env" => [
    %{name: "owner", value: address},
    %{name: "block.limit", value: 1},
    %{name: "token.price", value: 50_000},
    %{name: "validator.price", value: 100_000},
    %{name: "service.price", value: 100_000}
  ],
  "tokens" => [
    %Token{
      avatar: "https://ippan.com/images/IPN.png",
      created_at: 0,
      decimal: 9,
      id: "IPN",
      max_supply: 42_000_000_000_000_000,
      name: "Instant Payment Network",
      owner: nil,
      props: ["drop"],
      symbol: "Þ",
      updated_at: 0
    },
    %Token{
      avatar: "https://ippan.com/images/XPN.png",
      created_at: 0,
      decimal: 9,
      id: "XPN",
      max_supply: 0,
      name: "Xtream Payment Network",
      owner: address,
      props: ["burn", "drop", "lock", "reload", "stream"],
      symbol: "xÞ",
      updated_at: 0,
      env: %{
        "reload.amount" => 75000,
        "reload.times" => 15000,
        "reload.expiry" => 18250000,
        "stream.times" => 12
      }
    }
  ],
  "validators" => [
    %Validator{
      avatar: nil,
      created_at: 0,
      fa: 0,
      failures: 0,
      fb: 1,
      hostname: "ippan.co.uk",
      id: 0,
      name: "Master",
      net_pubkey: npk,
      owner: address,
      port: 5815,
      pubkey: pk,
      active: true,
      updated_at: 0
    }
  ],
  "accounts" => [
    %Account{
      id: "0x2Qyubpv2bgy8bDZ7UpkuwabHjJdc",
      pubkey: pk,
      sig_type: 0,
      vid: 0,
      fa: 0,
      fb: 1
    }
  ]
}
