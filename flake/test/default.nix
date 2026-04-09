{inputs, ...} @ parts: {
  imports = [
    # Broad integration tests exercising all major functionality end-to-end.
    # For example: complete workflows, system-wide behavior.
    ./integration.nix

    # Focused scenario tests for specific edge cases and curated situations.
    # For example: isolated scenarios, internal mechanisms.
    ./scenarios.nix
  ];
}
