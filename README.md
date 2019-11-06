Hedgehog Servant
================
Hedgehog servant will eat all your servant bugs.

Basic Usage
-----------
Define your servant API endpoints and automatically derive request generators
for them!

This is accomplished using the `genRequest` and the heterogeneous list, called
`GList`, that contains all the generators needed for your API.

So, what do we actually need to do?

1. Define our API (below `SimplestApi`).
2. Define generators for each element that gets captured in the request.
3. Call the function `genRequest` with a proxy for the API (`Proxy
   @SimplestApi`) and all generators needed in a generator list (`GList`)!

In code this looks like:

```haskell
-- POST /cats
type SimplestApi =
  "my" :> "cats" :> ReqBody '[JSON] Cat :> Post '[JSON] ()

-- Generate a request to the Cat API from a base URL
catRequestGen :: BaseUrl -> Gen Request
catRequestGen baseUrl =
  genRequest (Proxy @SimplestApi) (genCat :*: GNil) <&>
    \makeReq -> makeReq baseUrl
```

We construct generator lists with `element1 :*:
element2 :*: ... elementn :*: GNil`, where `GNil` denotes the end of the generator list. The
`genRequest` function will derive a request from the generators in the list.
This includes request bodies, headers and query parameters.

Note: since the generator list may contain many generators for a specific type,
the first one will be chosen. This means that for types that may collide (e.g.
common types like `String`,`Integer` etc), you should define newtype wrappers.
