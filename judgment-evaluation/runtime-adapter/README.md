# Chesstory runtime public-response adapter

`RuntimePublicResponseCli` is a development-only JSONL transport for the public
`RuntimeProtocol.evaluate` result. It passes each UTF-8 request line to the
runtime once and writes an object with exactly `http_status` and `body`, where
`body` is the unchanged `chesstory.move-meaning.response.v6` object from the
development-only `public-v6` schema.

```text
sbt -batch -error "runMain io.chesstory.evaluation.runtimeadapter.RuntimePublicResponseCli" < requests.jsonl > responses.jsonl
```

Requires JDK 21 and sbt 1.11.7. From this directory, `sbt -batch -error compile`
verifies the adapter, and `sbt -batch -error run` uses `RuntimePublicResponseCli`
as the configured main class.
