# Collaparte: concise Lilypond staff mirroring

Provides functionality for staves to copy music from a source staff whenever they would otherwise be empty. These staves are still treated as if they were empty for purposes of staff hiding. Primarily this is meant to be used to streamline the notation of string divisi, so that fully flexible line breaking can be possible while only entering notes for secondary staves where the parts actually divide.

Context properties are provided to filter which events are copied between staves.

Spanners beginning and ending in music for different staves connect together without warnings.

See `tests.ly` for usage example.

TO DO: more robust documentation and test coverage.

This code could easily be adapted for other purposes, such as sending dynamics or markings to a dedicated context.

## Technical notes

The handling of Dispatchers and context creation events via Scheme may be instructive.

The ability to handle spanners has potential applications in other areas. Notably, part-combine has open bugs relating to this behavior.

This implementation relies heavily on the ability to broadcast music-events during `pre-process-music`. This might be considered an abuse of the design as originally intended, but the fact that this works opens up lots of possibilities to do things efficiently during translation that otherwise would need to be done expensively by manipulating music expressions before translation. It might be a good idea ultimately to allow for multiple `pre-process-music`/`process-music` steps, similar to the way acknowledgers are handled.

Voices need to be kept alive in order to receive relayed music events, since as far as the iterator knows those voices are empty. Lilypond currently does not expose context-handles via Scheme, so the only way to do this without patching internals is to use a music function to generate skips to keep alive all voices that might need to be used, which is a mild performance hit. If not for that, `\collaParte` could be a context-mod instead of a music function.
