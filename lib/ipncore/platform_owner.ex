defmodule PlatformOwner do
  # seed
  # <<192, 198, 255, 103, 172, 14, 113, 243, 135, 19, 43, 1, 189, 146, 203, 162,
  # 197, 175, 196, 71, 66, 97, 116, 136, 228, 22, 123, 117, 121, 87, 42, 165, 163,
  # 215, 36, 207, 207, 152, 67, 166, 12, 43, 142, 237, 27, 77, 90, 177, 33, 19,
  # 176, 175, 248, 195, 13, 161, 180, 81, 91, 204, 239, 39, 45, 64>>

  def name, do: "IPPAN LTD"
  def token, do: "IPN"

  def pubkey do
    <<9, 39, 30, 11, 248, 74, 201, 79, 119, 165, 243, 119, 147, 212, 197, 16, 16, 184, 36, 145,
      138, 70, 93, 24, 157, 34, 2, 100, 119, 42, 201, 87, 21, 17, 36, 205, 6, 37, 87, 39, 68, 134,
      172, 13, 146, 113, 104, 110, 230, 213, 154, 220, 8, 186, 171, 17, 166, 2, 198, 60, 230, 165,
      27, 202, 127, 217, 54, 98, 193, 107, 86, 169, 232, 79, 182, 221, 100, 245, 100, 164, 105,
      170, 133, 27, 204, 42, 1, 243, 180, 119, 226, 188, 123, 133, 96, 123, 8, 43, 183, 173, 100,
      176, 83, 173, 196, 44, 58, 84, 184, 128, 188, 197, 24, 179, 237, 69, 129, 13, 152, 204, 81,
      157, 188, 151, 101, 143, 246, 181, 124, 6, 9, 84, 103, 190, 118, 68, 152, 52, 43, 197, 62,
      181, 248, 73, 177, 176, 215, 131, 1, 188, 41, 55, 43, 33, 113, 146, 190, 215, 134, 16, 45,
      37, 163, 12, 64, 49, 87, 100, 118, 100, 4, 81, 56, 120, 32, 21, 139, 10, 7, 235, 24, 200,
      214, 123, 186, 69, 132, 237, 155, 198, 141, 138, 2, 228, 40, 79, 244, 173, 182, 249, 5, 47,
      220, 128, 19, 170, 64, 251, 75, 206, 55, 32, 108, 104, 83, 130, 130, 139, 57, 57, 171, 200,
      223, 166, 29, 3, 29, 9, 88, 87, 5, 233, 38, 85, 232, 216, 230, 151, 166, 81, 214, 180, 149,
      172, 36, 27, 23, 201, 203, 43, 186, 206, 250, 53, 56, 225, 231, 151, 232, 141, 14, 130, 154,
      104, 242, 228, 122, 4, 252, 168, 233, 161, 195, 67, 48, 88, 211, 209, 30, 195, 178, 28, 109,
      232, 210, 78, 88, 21, 57, 164, 200, 170, 214, 202, 51, 82, 132, 144, 208, 77, 44, 90, 46,
      94, 152, 66, 227, 233, 52, 177, 70, 202, 99, 138, 218, 177, 15, 124, 107, 36, 210, 138, 247,
      111, 6, 209, 144, 180, 84, 90, 169, 178, 68, 66, 114, 1, 174, 115, 229, 34, 150, 101, 47,
      95, 85, 214, 131, 192, 112, 214, 25, 30, 97, 175, 194, 148, 235, 172, 33, 253, 135, 231, 6,
      46, 245, 132, 244, 84, 201, 52, 11, 195, 100, 93, 80, 53, 113, 6, 25, 51, 214, 199, 53, 107,
      26, 115, 130, 200, 206, 0, 164, 136, 202, 105, 134, 204, 4, 237, 201, 76, 154, 206, 202,
      209, 157, 210, 83, 87, 37, 208, 22, 80, 94, 152, 181, 200, 111, 249, 100, 203, 200, 139,
      149, 37, 104, 74, 196, 177, 40, 0, 90, 91, 15, 109, 33, 22, 116, 116, 90, 180, 162, 41, 41,
      204, 216, 146, 117, 230, 135, 30, 188, 47, 164, 54, 108, 142, 186, 124, 239, 192, 94, 2,
      235, 45, 12, 2, 97, 177, 23, 133, 81, 192, 148, 83, 38, 97, 119, 79, 36, 171, 102, 5, 32,
      27, 165, 157, 177, 120, 183, 111, 172, 63, 96, 186, 245, 255, 13, 140, 182, 109, 3, 230,
      154, 34, 98, 76, 84, 211, 23, 92, 169, 60, 72, 45, 34, 105, 21, 131, 230, 169, 138, 15, 1,
      178, 16, 242, 98, 144, 36, 240, 111, 26, 180, 44, 138, 223, 134, 195, 162, 30, 64, 149, 16,
      128, 47, 91, 235, 160, 230, 8, 84, 135, 24, 139, 40, 26, 41, 132, 225, 25, 245, 69, 68, 215,
      198, 43, 210, 72, 146, 221, 6, 26, 194, 153, 11, 55, 182, 16, 114, 75, 149, 212, 163, 104,
      72, 69, 104, 228, 4, 153, 228, 228, 114, 150, 25, 164, 25, 154, 209, 112, 209, 109, 32, 45,
      61, 2, 137, 157, 105, 121, 35, 164, 124, 243, 168, 197, 42, 1, 56, 222, 241, 235, 167, 37,
      27, 249, 158, 36, 108, 172, 241, 141, 56, 102, 201, 255, 70, 116, 174, 33, 249, 3, 10, 20,
      54, 44, 114, 88, 122, 49, 153, 171, 161, 236, 169, 148, 113, 228, 226, 45, 84, 162, 176,
      240, 41, 168, 148, 18, 242, 249, 188, 87, 84, 134, 28, 63, 200, 89, 43, 46, 94, 166, 224,
      131, 4, 73, 69, 65, 177, 104, 232, 63, 85, 236, 91, 48, 159, 250, 135, 219, 107, 173, 40,
      200, 229, 241, 166, 89, 2, 50, 197, 211, 88, 45, 159, 157, 64, 53, 87, 249, 4, 235, 142,
      228, 14, 51, 8, 8, 91, 113, 85, 77, 210, 208, 28, 47, 9, 249, 117, 86, 168, 209, 55, 163,
      53, 61, 160, 78, 169, 246, 25, 166, 106, 16, 172, 80, 240, 149, 234, 162, 180, 134, 76, 243,
      118, 56, 15, 203, 239, 91, 117, 63, 222, 165, 162, 55, 170, 119, 85, 62, 140, 65, 227, 236,
      68, 177, 161, 132, 213, 19, 106, 124, 42, 186, 151, 219, 38, 28, 233, 129, 74, 211, 83, 193,
      93, 216, 187, 26, 241, 39, 85, 214, 13, 68, 97, 122, 184, 205, 236, 195, 59, 32, 3, 43, 253,
      84, 113, 33, 18, 88, 211, 57, 147, 86, 103, 185, 113, 227, 176, 168, 11, 25, 108, 209, 22,
      110, 64, 133, 118, 214, 201, 251, 156, 109, 85, 5, 20, 198, 36, 25, 71, 15, 169, 120, 46,
      26, 224, 196, 19, 141, 56, 108, 171, 139, 88, 43, 148, 252, 228, 209, 156, 144, 212, 57, 29,
      254, 59, 63, 67, 11, 74, 100, 49, 16, 207, 207, 236>>
  end

  # only test
  def secret_key do
    <<89, 0, 14, 129, 39, 176, 197, 19, 176, 121, 0, 47, 64, 244, 78, 193, 8, 32, 124, 20, 0, 9,
      235, 127, 196, 232, 143, 188, 247, 224, 8, 23, 209, 129, 16, 96, 62, 15, 205, 250, 0, 112,
      65, 248, 191, 128, 244, 17, 255, 23, 161, 0, 244, 31, 184, 24, 65, 62, 240, 31, 251, 11,
      208, 5, 255, 224, 128, 39, 240, 126, 15, 209, 190, 239, 112, 253, 252, 130, 0, 4, 31, 133,
      252, 1, 131, 7, 240, 190, 24, 15, 196, 251, 255, 255, 239, 224, 63, 247, 240, 72, 252, 31,
      128, 23, 192, 2, 8, 143, 66, 4, 1, 128, 36, 96, 132, 7, 191, 253, 252, 63, 191, 0, 48, 250,
      23, 192, 2, 4, 32, 254, 231, 191, 254, 248, 0, 255, 244, 46, 192, 252, 14, 132, 0, 48, 61,
      7, 222, 130, 3, 194, 57, 248, 49, 201, 11, 209, 127, 12, 64, 67, 228, 49, 2, 235, 191, 131,
      248, 29, 250, 255, 208, 129, 243, 207, 62, 227, 208, 66, 248, 17, 127, 248, 95, 251, 4, 32,
      70, 15, 207, 65, 4, 14, 193, 4, 47, 131, 4, 32, 191, 4, 15, 250, 255, 242, 63, 8, 31, 190,
      255, 208, 0, 248, 0, 65, 7, 113, 63, 23, 223, 205, 7, 207, 68, 228, 64, 130, 24, 0, 132,
      247, 223, 192, 248, 0, 70, 251, 240, 64, 32, 63, 195, 15, 160, 76, 11, 223, 184, 252, 46,
      132, 236, 110, 132, 11, 157, 250, 3, 160, 255, 255, 255, 129, 15, 192, 0, 240, 0, 252, 252,
      82, 2, 252, 127, 125, 11, 209, 0, 239, 239, 188, 0, 64, 1, 0, 47, 197, 0, 79, 126, 0, 32, 2,
      0, 143, 190, 248, 47, 6, 239, 176, 64, 0, 127, 131, 8, 31, 194, 252, 97, 57, 4, 16, 193,
      243, 191, 194, 247, 191, 66, 12, 32, 4, 23, 239, 63, 240, 17, 61, 4, 49, 1, 255, 240, 196,
      12, 0, 191, 231, 176, 129, 15, 240, 132, 27, 142, 195, 39, 128, 253, 236, 30, 184, 20, 64,
      1, 252, 30, 196, 24, 112, 128, 252, 47, 190, 4, 15, 62, 232, 47, 128, 224, 111, 134, 7, 208,
      190, 252, 80, 1, 236, 79, 188, 4, 81, 67, 3, 160, 127, 232, 65, 70, 0, 47, 69, 19, 191, 249,
      224, 31, 194, 251, 192, 129, 4, 110, 193, 3, 206, 194, 243, 175, 253, 11, 207, 1, 252, 48,
      127, 4, 64, 128, 3, 191, 65, 248, 31, 193, 12, 31, 254, 251, 224, 126, 255, 205, 121, 12,
      94, 194, 251, 206, 254, 28, 63, 64, 19, 207, 193, 243, 239, 130, 16, 49, 130, 252, 30, 198,
      20, 127, 70, 235, 81, 130, 7, 238, 126, 11, 255, 2, 8, 15, 130, 248, 14, 190, 244, 32, 128,
      19, 237, 133, 28, 47, 65, 248, 17, 123, 0, 46, 197, 20, 47, 65, 235, 192, 189, 232, 15, 123,
      20, 47, 3, 3, 160, 71, 27, 255, 253, 232, 79, 58, 4, 0, 68, 255, 223, 253, 19, 222, 181,
      252, 2, 195, 255, 255, 68, 243, 255, 123, 4, 97, 192, 23, 128, 59, 243, 159, 125, 232, 95,
      70, 3, 191, 64, 232, 159, 255, 0, 79, 3, 255, 175, 128, 243, 225, 72, 255, 240, 192, 248,
      16, 126, 0, 64, 62, 252, 31, 4, 0, 63, 252, 11, 240, 254, 0, 31, 9, 243, 225, 196, 11, 174,
      197, 255, 144, 133, 31, 225, 129, 231, 207, 125, 227, 144, 1, 31, 192, 195, 243, 224, 192,
      243, 160, 125, 239, 242, 2, 228, 31, 189, 240, 14, 248, 248, 143, 190, 19, 208, 128, 24, 49,
      249, 252, 0, 61, 3, 223, 64, 16, 111, 246, 8, 0, 64, 16, 46, 126, 32, 30, 252, 235, 144,
      191, 12, 95, 195, 4, 49, 127, 3, 224, 61, 12, 33, 124, 7, 222, 251, 15, 145, 129, 7, 191,
      255, 15, 255, 1, 8, 0, 134, 248, 45, 194, 247, 239, 129, 244, 64, 186, 7, 222, 199, 20, 146,
      62, 11, 79, 126, 228, 80, 124, 224, 47, 199, 239, 225, 3, 235, 111, 63, 227, 178, 62, 32,
      63, 129, 19, 255, 123, 227, 241, 127, 228, 190, 132, 248, 16, 64, 255, 176, 64, 24, 15, 58,
      4, 63, 69, 0, 32, 125, 208, 80, 253, 0, 0, 199, 236, 15, 59, 24, 0, 65, 8, 45, 191, 0, 15,
      191, 19, 208, 125, 255, 222, 250, 251, 190, 64, 232, 126, 196, 236, 0, 195, 239, 176, 250,
      252, 31, 134, 229, 253, 33, 45, 21, 246, 224, 253, 6, 230, 30, 231, 222, 13, 33, 25, 248,
      14, 17, 220, 1, 238, 245, 15, 209, 213, 249, 22, 233, 9, 66, 198, 233, 244, 219, 50, 2, 249,
      238, 16, 30, 251, 24, 229, 215, 250, 3, 226, 4, 12, 28, 57, 250, 236, 231, 35, 11, 0, 233,
      10, 16, 3, 224, 47, 14, 38, 254, 240, 17, 20, 4, 61, 252, 9, 233, 15, 199, 212, 254, 241,
      72, 10, 17, 236, 21, 10, 237, 67, 229, 238, 232, 8, 2, 245, 233, 227, 15, 26, 3, 36, 19, 5,
      14, 227, 247, 7, 4, 244, 4, 249, 219, 234, 18, 14, 0, 252, 31, 32, 10, 216, 24, 247, 4, 29,
      245, 6, 220, 3, 239, 254, 22, 10, 228, 227, 3, 2, 38, 36, 210, 238, 19, 205, 223, 23, 253,
      0, 0, 19, 9, 22, 20, 242, 245, 215, 246, 65, 2, 246, 36, 0, 247, 13, 245, 17, 247, 8, 24,
      247, 17, 16, 254, 223, 220, 242, 19, 29, 15, 252, 200, 9, 233, 32, 249, 246, 5, 8, 211, 2,
      246, 6, 40, 215, 237, 254, 7, 20, 255, 2, 231, 8, 250, 27, 24, 12, 239, 52, 236, 14, 23, 17,
      250, 255, 10, 245, 227, 247, 251, 87, 14, 216, 247, 17, 239, 207, 5, 32, 20, 252, 237, 227,
      5, 241, 226, 14, 19, 206, 246, 255, 235, 249, 251, 230, 2, 239, 16, 61, 250, 3, 208, 240,
      254, 247, 31, 13, 7, 245, 241, 11, 232, 254, 254, 248, 37, 14, 20, 5, 248, 14, 14, 255, 36,
      232, 8, 11, 20, 211, 0, 236, 248, 19, 228, 215, 10, 11, 10, 244, 3, 252, 227, 34, 220, 205,
      6, 219, 35, 19, 247, 29, 9, 249, 194, 232, 17, 17, 6, 6, 244, 246, 4, 42, 1, 3, 32, 49, 248,
      59, 234, 5, 254, 242, 244, 242, 4, 15, 44, 16, 2, 15, 9, 57, 33, 246, 215, 236, 254, 218,
      238, 23, 5, 18, 230, 254, 35, 255, 252, 232, 223, 236, 235, 11, 2, 7, 235, 249, 48, 241, 24,
      27, 2, 233, 238, 214, 38, 14, 241, 242, 247, 247, 212, 22, 4, 36, 3, 15, 254, 246, 16, 230,
      6, 245, 39, 2, 21, 231, 18, 218, 224, 5, 253, 22, 7, 9, 51, 198, 20, 235, 43, 248, 12, 16,
      243, 9, 187, 223, 246, 251, 220, 43, 230, 1, 18, 253, 42, 245, 232, 27, 251, 245, 193, 251,
      8, 14, 241, 32, 25, 33, 250, 241, 253, 250, 245, 1, 254, 53, 27, 7, 11, 19, 228, 242, 245,
      242, 237, 24, 247, 232, 16, 7, 22, 39, 221, 41, 31, 237, 246, 232, 1, 23, 241, 1, 226, 248,
      46, 228, 38, 12, 43, 246, 10, 252, 245, 0, 226, 220, 234, 214, 24, 241, 34, 231, 219, 21,
      247, 9, 244, 240, 200, 242, 252, 197, 30, 249, 11, 19, 21, 222, 42, 3, 11, 7, 20, 28, 44,
      249, 244, 249, 226, 222, 244, 8, 20, 13>>
  end

  def address, do: Address.to_internal_address(pubkey())
end
