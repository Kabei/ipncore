defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  # test "json" do
  #   data = %{
  #     asd: 5,
  #     asdsad: "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyasdasdsad",
  #     poasd: 165_844,
  #     asdsad:
  #       "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLGCYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLGCYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG",
  #     tiestamp: 1_683_693_513_649,
  #     wer: 513_551_351,
  #     poaspdsd: [5, 5, 85646]
  #   }

  #   # data = [215315, 5385, "asdsdsadasd"]

  #   # data = [
  #   #   500,
  #   #   1_683_693_513_649,
  #   #   "kambei",
  #   #   [
  #   #     505.16803548,
  #   #     "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG+sGiyDkii0khK4EHUqgL9OR1dZ0oZPBnpUNQo3DT47CgdPvXsKMWhki3ipshmbF5j3SuJe+PKokBVFx2XVAkutMlqoqh8HObhaguQXmYoEGPfbI9YVtQhkhEJliZ4NJBfePTxFwcgwx6ZMyKSAWRGPkigWYY4H97pGf6vcVP29dXlWXcrDmz0LluEnurOstHbK4sRO9EwXu4GPBemct6zCG6XIosKvUcdWVEK2zloOdSWgGhkJUix2NdckbS10sExmLWVFdtSmM7aRE2QwXBQp3MVzBCRGEBa3SyAUwzGfXIpCf6v2au6Hdo9nUMG7SFUZIi8SbREo54fITdCtOdDIaELSkb3P01lGVR04C8KoiAYNKuwS9ewDD5wwemDDmQKgZpqlI3EwAhFgNqymY6J0WFcrnHH9/TGR2safDEb43EsxrdUiylaG235KokChL4o+akL7IartFLAB+CiOrK6gm8Um50WbMZ6ZGqtuMmVkQcmP/ZYSS1kf7+692HjWQ02eNs28FS0O/TxN8WOtKXJPHnIQ+uGiuBDfWXwYNxcMjavpFkgZbrzhnKPCBeS7iRBHY2ocYZseKGRJRho1PU2Fj1z3lcbWc1TYzbqTDM8g4ZHzb152DCRvKhGWvoKR3IKRLXY9yR4jZMqVV6YLdgqF86Imj1TIIqaTteA6H5juApjFVNFvyYRWhYOTqFt5XGmYDmziGe7MIf0PRaNcM1MVT5irzHpAnRHIu+w8K9aAvRMwO4q4jLF45ZrpCYYzAC5a236T0Iv0pAfkKFp6uAk34q1ZgtlJ9L3Evz+RbBFFpiY8qDzSEuxqn7gpNTVJWG1iUROn2PpwHVC37Yp6MIGgs5GN2BbcvUJ07kkrxtC3XWNpezDr+WGFcqZgi1UfZBO5+AlI7miaPkWnVYSYA35IxLFtoRw0NngUjNtrEkrw0WIjhhpCmEMQic5Iva35NMw9Ou6+y6WeYkw4hPWMEW4rJcFi/KSgEbQmQr7Eqy6JiisMFnRKwh9LFwGVcfLDVksuyoDq/NakLQgH3oEzLwnVoY6wwIrTvU5S0VZ+7vV0Jg2kgsyDEDVyp/7IE0rax2",
  #   #     "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG+sGiyDkii0khK4EHUqgL9OR1dZ0oZPBnpUNQo3DT47CgdPvXsKMWhki3ipshmbF5j3SuJe+PKokBVFx2XVAkutMlqoqh8HObhaguQXmYoEGPfbI9YVtQhkhEJliZ4NJBfePTxFwcgwx6ZMyKSAWRGPkigWYY4H97pGf6vcVP29dXlWXcrDmz0LluEnurOstHbK4sRO9EwXu4GPBemct6zCG6XIosKvUcdWVEK2zloOdSWgGhkJUix2NdckbS10sExmLWVFdtSmM7aRE2QwXBQp3MVzBCRGEBa3SyAUwzGfXIpCf6v2au6Hdo9nUMG7SFUZIi8SbREo54fITdCtOdDIaELSkb3P01lGVR04C8KoiAYNKuwS9ewDD5wwemDDmQKgZpqlI3EwAhFgNqymY6J0WFcrnHH9/TGR2safDEb43EsxrdUiylaG235KokChL4o+akL7IartFLAB+CiOrK6gm8Um50WbMZ6ZGqtuMmVkQcmP/ZYSS1kf7+692HjWQ02eNs28FS0O/TxN8WOtKXJPHnIQ+uGiuBDfWXwYNxcMjavpFkgZbrzhnKPCBeS7iRBHY2ocYZseKGRJRho1PU2Fj1z3lcbWc1TYzbqTDM8g4ZHzb152DCRvKhGWvoKR3IKRLXY9yR4jZMqVV6YLdgqF86Imj1TIIqaTteA6H5juApjFVNFvyYRWhYOTqFt5XGmYDmziGe7MIf0PRaNcM1MVT5irzHpAnRHIu+w8K9aAvRMwO4q4jLF45ZrpCYYzAC5a236T0Iv0pAfkKFp6uAk34q1ZgtlJ9L3Evz+RbBFFpiY8qDzSEuxqn7gpNTVJWG1iUROn2PpwHVC37Yp6MIGgs5GN2BbcvUJ07kkrxtC3XWNpezDr+WGFcqZgi1UfZBO5+AlI7miaPkWnVYSYA35IxLFtoRw0NngUjNtrEkrw0WIjhhpCmEMQic5Iva35NMw9Ou6+y6WeYkw4hPWMEW4rJcFi/KSgEbQmQr7Eqy6JiisMFnRKwh9LFwGVcfLDVksuyoDq/NakLQgH3oEzLwnVoY6wwIrTvU5S0VZ+7vV0Jg2kgsyDEDVyp/7IE0rax2",
  #   #     "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG+sGiyDkii0khK4EHUqgL9OR1dZ0oZPBnpUNQo3DT47CgdPvXsKMWhki3ipshmbF5j3SuJe+PKokBVFx2XVAkutMlqoqh8HObhaguQXmYoEGPfbI9YVtQhkhEJliZ4NJBfePTxFwcgwx6ZMyKSAWRGPkigWYY4H97pGf6vcVP29dXlWXcrDmz0LluEnurOstHbK4sRO9EwXu4GPBemct6zCG6XIosKvUcdWVEK2zloOdSWgGhkJUix2NdckbS10sExmLWVFdtSmM7aRE2QwXBQp3MVzBCRGEBa3SyAUwzGfXIpCf6v2au6Hdo9nUMG7SFUZIi8SbREo54fITdCtOdDIaELSkb3P01lGVR04C8KoiAYNKuwS9ewDD5wwemDDmQKgZpqlI3EwAhFgNqymY6J0WFcrnHH9/TGR2safDEb43EsxrdUiylaG235KokChL4o+akL7IartFLAB+CiOrK6gm8Um50WbMZ6ZGqtuMmVkQcmP/ZYSS1kf7+692HjWQ02eNs28FS0O/TxN8WOtKXJPHnIQ+uGiuBDfWXwYNxcMjavpFkgZbrzhnKPCBeS7iRBHY2ocYZseKGRJRho1PU2Fj1z3lcbWc1TYzbqTDM8g4ZHzb152DCRvKhGWvoKR3IKRLXY9yR4jZMqVV6YLdgqF86Imj1TIIqaTteA6H5juApjFVNFvyYRWhYOTqFt5XGmYDmziGe7MIf0PRaNcM1MVT5irzHpAnRHIu+w8K9aAvRMwO4q4jLF45ZrpCYYzAC5a236T0Iv0pAfkKFp6uAk34q1ZgtlJ9L3Evz+RbBFFpiY8qDzSEuxqn7gpNTVJWG1iUROn2PpwHVC37Yp6MIGgs5GN2BbcvUJ07kkrxtC3XWNpezDr+WGFcqZgi1UfZBO5+AlI7miaPkWnVYSYA35IxLFtoRw0NngUjNtrEkrw0WIjhhpCmEMQic5Iva35NMw9Ou6+y6WeYkw4hPWMEW4rJcFi/KSgEbQmQr7Eqy6JiisMFnRKwh9LFwGVcfLDVksuyoDq/NakLQgH3oEzLwnVoY6wwIrTvU5S0VZ+7vV0Jg2kgsyDEDVyp/7IE0rax2"
  #   #   ]
  #   # ]

  #   Benchee.run(%{
  #     "jason" => fn -> Jason.encode!(data) end,
  #     "poison" => fn -> Poison.encode!(data) end,
  #     "thoas" => fn -> :thoas.encode(data) end,
  #     "jsonrs" => fn -> Jsonrs.encode(data) end
  #   })
  # end

  # test "json deocde" do
  #   # json = """
  #   #   [
  #   #   0,
  #   #   1683693513649,
  #   #   "kambei",
  #   #     505.16803548,
  #   #     "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG+sGiyDkii0khK4EHUqgL9OR1dZ0oZPBnpUNQo3DT47CgdPvXsKMWhki3ipshmbF5j3SuJe+PKokBVFx2XVAkutMlqoqh8HObhaguQXmYoEGPfbI9YVtQhkhEJliZ4NJBfePTxFwcgwx6ZMyKSAWRGPkigWYY4H97pGf6vcVP29dXlWXcrDmz0LluEnurOstHbK4sRO9EwXu4GPBemct6zCG6XIosKvUcdWVEK2zloOdSWgGhkJUix2NdckbS10sExmLWVFdtSmM7aRE2QwXBQp3MVzBCRGEBa3SyAUwzGfXIpCf6v2au6Hdo9nUMG7SFUZIi8SbREo54fITdCtOdDIaELSkb3P01lGVR04C8KoiAYNKuwS9ewDD5wwemDDmQKgZpqlI3EwAhFgNqymY6J0WFcrnHH9/TGR2safDEb43EsxrdUiylaG235KokChL4o+akL7IartFLAB+CiOrK6gm8Um50WbMZ6ZGqtuMmVkQcmP/ZYSS1kf7+692HjWQ02eNs28FS0O/TxN8WOtKXJPHnIQ+uGiuBDfWXwYNxcMjavpFkgZbrzhnKPCBeS7iRBHY2ocYZseKGRJRho1PU2Fj1z3lcbWc1TYzbqTDM8g4ZHzb152DCRvKhGWvoKR3IKRLXY9yR4jZMqVV6YLdgqF86Imj1TIIqaTteA6H5juApjFVNFvyYRWhYOTqFt5XGmYDmziGe7MIf0PRaNcM1MVT5irzHpAnRHIu+w8K9aAvRMwO4q4jLF45ZrpCYYzAC5a236T0Iv0pAfkKFp6uAk34q1ZgtlJ9L3Evz+RbBFFpiY8qDzSEuxqn7gpNTVJWG1iUROn2PpwHVC37Yp6MIGgs5GN2BbcvUJ07kkrxtC3XWNpezDr+WGFcqZgi1UfZBO5+AlI7miaPkWnVYSYA35IxLFtoRw0NngUjNtrEkrw0WIjhhpCmEMQic5Iva35NMw9Ou6+y6WeYkw4hPWMEW4rJcFi/KSgEbQmQr7Eqy6JiisMFnRKwh9LFwGVcfLDVksuyoDq/NakLQgH3oEzLwnVoY6wwIrTvU5S0VZ+7vV0Jg2kgsyDEDVyp/7IE0rax2"
  #   # ]
  #   # """

  #   json = """
  #   {
  #     "type": 100,
  #     "timestamp": 1683693513649,
  #     "args": {
  #         "method": 505.1535,
  #         "params": "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG+sGiyDkii0khK4EHUqgL9OR1dZ0oZPBnpUNQo3DT47CgdPvXsKMWhki3ipshmbF5j3SuJe+PKokBVFx2XVAkutMlqoqh8HObhaguQXmYoEGPfbI9YVtQhkhEJliZ4NJBfePTxFwcgwx6ZMyKSAWRGPkigWYY4H97pGf6vcVP29dXlWXcrDmz0LluEnurOstHbK4sRO9EwXu4GPBemct6zCG6XIosKvUcdWVEK2zloOdSWgGhkJUix2NdckbS10sExmLWVFdtSmM7aRE2QwXBQp3MVzBCRGEBa3SyAUwzGfXIpCf6v2au6Hdo9nUMG7SFUZIi8SbREo54fITdCtOdDIaELSkb3P01lGVR04C8KoiAYNKuwS9ewDD5wwemDDmQKgZpqlI3EwAhFgNqymY6J0WFcrnHH9/TGR2safDEb43EsxrdUiylaG235KokChL4o+akL7IartFLAB+CiOrK6gm8Um50WbMZ6ZGqtuMmVkQcmP/ZYSS1kf7+692HjWQ02eNs28FS0O/TxN8WOtKXJPHnIQ+uGiuBDfWXwYNxcMjavpFkgZbrzhnKPCBeS7iRBHY2ocYZseKGRJRho1PU2Fj1z3lcbWc1TYzbqTDM8g4ZHzb152DCRvKhGWvoKR3IKRLXY9yR4jZMqVV6YLdgqF86Imj1TIIqaTteA6H5juApjFVNFvyYRWhYOTqFt5XGmYDmziGe7MIf0PRaNcM1MVT5irzHpAnRHIu+w8K9aAvRMwO4q4jLF45ZrpCYYzAC5a236T0Iv0pAfkKFp6uAk34q1ZgtlJ9L3Evz+RbBFFpiY8qDzSEuxqn7gpNTVJWG1iUROn2PpwHVC37Yp6MIGgs5GN2BbcvUJ07kkrxtC3XWNpezDr+WGFcqZgi1UfZBO5+AlI7miaPkWnVYSYA35IxLFtoRw0NngUjNtrEkrw0WIjhhpCmEMQic5Iva35NMw9Ou6+y6WeYkw4hPWMEW4rJcFi/KSgEbQmQr7Eqy6JiisMFnRKwh9LFwGVcfLDVksuyoDq/NakLQgH3oEzLwnVoY6wwIrTvU5S0VZ+7vV0Jg2kgsyDEDVyp/7IE0rax2",
  #         "validator": "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG+sGiyDkii0khK4EHUqgL9OR1dZ0oZPBnpUNQo3DT47CgdPvXsKMWhki3ipshmbF5j3SuJe+PKokBVFx2XVAkutMlqoqh8HObhaguQXmYoEGPfbI9YVtQhkhEJliZ4NJBfePTxFwcgwx6ZMyKSAWRGPkigWYY4H97pGf6vcVP29dXlWXcrDmz0LluEnurOstHbK4sRO9EwXu4GPBemct6zCG6XIosKvUcdWVEK2zloOdSWgGhkJUix2NdckbS10sExmLWVFdtSmM7aRE2QwXBQp3MVzBCRGEBa3SyAUwzGfXIpCf6v2au6Hdo9nUMG7SFUZIi8SbREo54fITdCtOdDIaELSkb3P01lGVR04C8KoiAYNKuwS9ewDD5wwemDDmQKgZpqlI3EwAhFgNqymY6J0WFcrnHH9/TGR2safDEb43EsxrdUiylaG235KokChL4o+akL7IartFLAB+CiOrK6gm8Um50WbMZ6ZGqtuMmVkQcmP/ZYSS1kf7+692HjWQ02eNs28FS0O/TxN8WOtKXJPHnIQ+uGiuBDfWXwYNxcMjavpFkgZbrzhnKPCBeS7iRBHY2ocYZseKGRJRho1PU2Fj1z3lcbWc1TYzbqTDM8g4ZHzb152DCRvKhGWvoKR3IKRLXY9yR4jZMqVV6YLdgqF86Imj1TIIqaTteA6H5juApjFVNFvyYRWhYOTqFt5XGmYDmziGe7MIf0PRaNcM1MVT5irzHpAnRHIu+w8K9aAvRMwO4q4jLF45ZrpCYYzAC5a236T0Iv0pAfkKFp6uAk34q1ZgtlJ9L3Evz+RbBFFpiY8qDzSEuxqn7gpNTVJWG1iUROn2PpwHVC37Yp6MIGgs5GN2BbcvUJ07kkrxtC3XWNpezDr+WGFcqZgi1UfZBO5+AlI7miaPkWnVYSYA35IxLFtoRw0NngUjNtrEkrw0WIjhhpCmEMQic5Iva35NMw9Ou6+y6WeYkw4hPWMEW4rJcFi/KSgEbQmQr7Eqy6JiisMFnRKwh9LFwGVcfLDVksuyoDq/NakLQgH3oEzLwnVoY6wwIrTvU5S0VZ+7vV0Jg2kgsyDEDVyp/7IE0rax2"
  #     },
  #     "auth": "CYEhHWMLavdMQGDy2kWDavVX0avtKy4iaXGBGxWxtXAAyiShPq6Qa6VOQWgmBzBTaHpe0OTynuZ3Q4q8J5Nr2zqy5WPSFdCHcZq5OEBIXpLG+sGiyDkii0khK4EHUqgL9OR1dZ0oZPBnpUNQo3DT47CgdPvXsKMWhki3ipshmbF5j3SuJe+PKokBVFx2XVAkutMlqoqh8HObhaguQXmYoEGPfbI9YVtQhkhEJliZ4NJBfePTxFwcgwx6ZMyKSAWRGPkigWYY4H97pGf6vcVP29dXlWXcrDmz0LluEnurOstHbK4sRO9EwXu4GPBemct6zCG6XIosKvUcdWVEK2zloOdSWgGhkJUix2NdckbS10sExmLWVFdtSmM7aRE2QwXBQp3MVzBCRGEBa3SyAUwzGfXIpCf6v2au6Hdo9nUMG7SFUZIi8SbREo54fITdCtOdDIaELSkb3P01lGVR04C8KoiAYNKuwS9ewDD5wwemDDmQKgZpqlI3EwAhFgNqymY6J0WFcrnHH9/TGR2safDEb43EsxrdUiylaG235KokChL4o+akL7IartFLAB+CiOrK6gm8Um50WbMZ6ZGqtuMmVkQcmP/ZYSS1kf7+692HjWQ02eNs28FS0O/TxN8WOtKXJPHnIQ+uGiuBDfWXwYNxcMjavpFkgZbrzhnKPCBeS7iRBHY2ocYZseKGRJRho1PU2Fj1z3lcbWc1TYzbqTDM8g4ZHzb152DCRvKhGWvoKR3IKRLXY9yR4jZMqVV6YLdgqF86Imj1TIIqaTteA6H5juApjFVNFvyYRWhYOTqFt5XGmYDmziGe7MIf0PRaNcM1MVT5irzHpAnRHIu+w8K9aAvRMwO4q4jLF45ZrpCYYzAC5a236T0Iv0pAfkKFp6uAk34q1ZgtlJ9L3Evz+RbBFFpiY8qDzSEuxqn7gpNTVJWG1iUROn2PpwHVC37Yp6MIGgs5GN2BbcvUJ07kkrxtC3XWNpezDr+WGFcqZgi1UfZBO5+AlI7miaPkWnVYSYA35IxLFtoRw0NngUjNtrEkrw0WIjhhpCmEMQic5Iva35NMw9Ou6+y6WeYkw4hPWMEW4rJcFi/KSgEbQmQr7Eqy6JiisMFnRKwh9LFwGVcfLDVksuyoDq/NakLQgH3oEzLwnVoY6wwIrTvU5S0VZ+7vV0Jg2kgsyDEDVyp/7IE0rax2"
  #   }
  #   """

  #   Benchee.run(%{
  #     "jason" => fn -> Jason.decode!(json) end,
  #     "poison" => fn -> Poison.decode!(json) end,
  #     "thoas" => fn -> :thoas.decode(json) end,
  #     "jsonrs" => fn -> Jsonrs.decode(json) end
  #   })
  # end

  test "base" do
    data16 = :crypto.strong_rand_bytes(32) |> Base.encode16()
    data64 = :crypto.strong_rand_bytes(32) |> Base.encode64()

    Benchee.run(%{
      "base16" => fn ->
        Base.decode16!(data16)
      end,
      "base64" => fn ->
        Base.decode64!(data64)
      end,
      "fast64" => fn ->
        Fast64.decode64(data64)
      end
    })
  end

  # test "falcon" do
  #   seed = :crypto.strong_rand_bytes(32)
  #   msg = :crypto.strong_rand_bytes(32)
  #   {:ok, pub, sk} = Falcon.gen_keys_from_seed(seed)
  #   {:ok, sig} = Falcon.sign(sk, msg)

  #   Benchee.run(%{
  #     "falcon" => fn ->
  #       Falcon.verify(msg, sig, pub)
  #     end
  #   })
  # end

  # test "map" do
  #   hash = :crypto.strong_rand_bytes(32)

  #   Benchee.run(%{
  #     "raw" => fn ->
  #       %{"hash" => Base.encode16(hash), "asd" => hash}
  #     end,
  #     "map" => fn ->
  #       Map.put(%{}, "hash", Base.encode16(hash))
  #     end
  #   })
  # end

  # test "list_to_tuple" do
  #   data = [
  #     51,
  #     1640_154_148,
  #     [
  #       "MfNh7cwvB9F/CGRfFO+bgd3DmJbzNIIjb6mdSzOKK9wzXtBlLZ/k3JfBZj3EBBhxA+K4AVtAg+32Rs5r8ubjG10TPBfZWmQrRuwLc91OHEpx2mqs5Xs2TxyCOjY7F/Vml8hIDejfUt8oeqWhxXED79CDUZkP4Z7Ac7lAnsViZl8HyUiwOTHF9JHOmsU87ezmcNUay7HgjKKK4Rp3LOL8GlwzbkLK0bax0KOi2lLGPrG5TWwBqKy6yJXG+buuRgzh1AuSgpVLNSyNu60f3se5vQRbPVY+o7cL5IeADBZqOcYaYoEb4W5k21sS1KBSRiWSCV7P/9A6XUYWGBhHShAtTMnLAerc7PW8cyF2kqRwnXHjvJR8ixrr26YTtZdkuWSLjfsHZYmfGHvT6v9/FOjlskRIGTeYDS1hj4rtorFZmlQkiBsBtH/DE8kRYinXGtZK5yAFbu6T4XOy7uelt0goW+7XjBu6vD04Q+dVPpsjdCC4KvbwipuEZtPDTQoXueyfGe/e69lkTaG5R9NvoGzAERCgtqBcYGekvdEXnLVA96BA1f39WX2BjWEG+zKYVg3PLozBDuQTq35Ad2SGk5w4iNLnfKOGzIFIl7X6pDImXoaOzJrLhkOh+FLuzIUlWXjrqSTZDGqqTpnLKqlKxjMUBT11LrQJlGoOCG+etiQcuNSMQpGnCpsAH0EZH4bexSeMPVcBWEvHWxkieGuDtM48jE2/jQtyL2uEpKA6WrVO6saWtnsHdWBSGeTFMXsDCCx8bSfY2BMKo9LD2K3YL4y0a0O3jhdmwWcMEHjA69Cg0c0+qZ/vvjLpudRm8J5AkTSCzA==",
  #       "IPN",
  #       50_000
  #     ]
  #   ]

  #   [a, b, c] = data
  #   d = :crypto.strong_rand_bytes(625)

  #   Benchee.run(%{
  #     "1" => fn ->
  #       List.to_tuple(data)
  #       |> Tuple.append(d)
  #     end,
  #     "2" => fn ->
  #       {a, b, c, d}
  #     end
  #   })
  # end

  # test "hashes" do
  #   data = :crypto.strong_rand_bytes(2000)

  #   req =
  #     {50, [5, <<45::256>>],
  #      "MfNh7cwvB9F/CGRfFO+bgd3DmJbzNIIjb6mdSzOKK9wzXtBlLZ/k3JfBZj3EBBhxA+K4AVtAg+32Rs5r8ubjG10TPBfZWmQrRuwLc91OHEpx2mqs5Xs2TxyCOjY7F/Vml8hIDejfUt8oeqWhxXED79CDUZkP4Z7Ac7lAnsViZl8HyUiwOTHF9JHOmsU87ezmcNUay7HgjKKK4Rp3LOL8GlwzbkLK0bax0KOi2lLGPrG5TWwBqKy6yJXG+buuRgzh1AuSgpVLNSyNu60f3se5vQRbPVY+o7cL5IeADBZqOcYaYoEb4W5k21sS1KBSRiWSCV7P/9A6XUYWGBhHShAtTMnLAerc7PW8cyF2kqRwnXHjvJR8ixrr26YTtZdkuWSLjfsHZYmfGHvT6v9/FOjlskRIGTeYDS1hj4rtorFZmlQkiBsBtH/DE8kRYinXGtZK5yAFbu6T4XOy7uelt0goW+7XjBu6vD04Q+dVPpsjdCC4KvbwipuEZtPDTQoXueyfGe/e69lkTaG5R9NvoGzAERCgtqBcYGekvdEXnLVA96BA1f39WX2BjWEG+zKYVg3PLozBDuQTq35Ad2SGk5w4iNLnfKOGzIFIl7X6pDImXoaOzJrLhkOh+FLuzIUlWXjrqSTZDGqqTpnLKqlKxjMUBT11LrQJlGoOCG+etiQcuNSMQpGnCpsAH0EZH4bexSeMPVcBWEvHWxkieGuDtM48jE2/jQtyL2uEpKA6WrVO6saWtnsHdWBSGeTFMXsDCCx8bSfY2BMKo9LD2K3YL4y0a0O3jhdmwWcMEHjA69Cg0c0+qZ/vvjLpudRm8J5AkTSCzA=="}

  #   Benchee.run(%{
  #     "blake3" => fn ->
  #       Blake3.hash(data)
  #     end,
  #     "blake3-2" => fn ->
  #       {type, arg, sig} = req

  #       Enum.reduce(arg, "", fn x, acc -> :binary.list_to_bin([acc, "#{x}"]) end)

  #       Blake3.hash("#{type}#{arg}#{Base.decode64!(sig)}")
  #     end
  #   })
  # end

  # test "base64" do
  #   a = Base.encode16(:crypto.strong_rand_bytes(897))
  #   b = Base.encode16(:crypto.strong_rand_bytes(32))
  #   c = Base.encode16(:crypto.strong_rand_bytes(32))
  #   data = a <> b <> c

  #   Benchee.run(%{
  #     "separate" => fn ->
  #       Base.decode16!(a)
  #       Base.decode16!(b)
  #       Base.decode16!(c)
  #     end,
  #     "one" => fn ->
  #       x = Base.decode16!(data)
  #       <<_a1::bytes-size(897), _::bytes-size(32), _::bytes-size(32)>> = x
  #     end
  #   })
  # end
end
