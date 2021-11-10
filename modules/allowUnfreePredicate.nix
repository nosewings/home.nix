{ config, lib, ... }:
with lib;
with types;
let
  predicate = mkOptionType rec {
    name = "predicate";
    description = "predicate";
    check = isFunction;
    merge = _: foldr (def: acc: x: def.value x || acc x) emptyValue;
    emptyValue = const false;
  };
in
{
  options.allowUnfreePredicate = mkOption {
    type = nullOr predicate;
    default = predicate.emptyValue;
  };

  config = mkIf (config.allowUnfreePredicate != null) {
    nixpkgs.config.allowUnfreePredicate = config.allowUnfreePredicate;
  };
}
