{ lib }: with lib; {
enableMultiple =
list: lib.genAttrs list (x: { enable = mkDefault true; });
}
