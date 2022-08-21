# Revision history for file-embed-lzma

## 0.0.1

- Use `bytesPrimL` when available
- Drop GHC-7.x support.
  We no more depend on `th-lift-instances`,
  and use `TemplateHaskellQuotes` instead of `TemplateHaskell` for the lib
  itself.
- `embedRecursiveDir` sorts files, so the generated code is more deterministic

## 0

- First version. Released on an unsuspecting world.
