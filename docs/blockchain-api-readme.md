## Response behaviour

Code|Response
-|-
200|Ok
400|Bad request
404|Not found
409|Conflict

### Retrives a List of data:

 `GET /tokens`

Code|Result
-|-
200|Retrives a json array
204|No data

### Retrives a single data

 `GET /tokens/IPN`

Code|Result
-|-
200| Retrives a json object
204| No data

### Head requests

 `HEAD /tokens/IPN`

Code|Meaning
-|-
200| No Exists / Available
409| Exists / Not Available

## Continuous Paging
To perform continuous paging, data is requested using limit and start parameters in the request.

The following conditions are used to detect the end of paging:
- If the number of items returned differs from the limit.
- When the response is code 204
