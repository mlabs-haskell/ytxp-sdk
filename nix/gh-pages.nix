{ inputs, ... }: {
  imports = [
    inputs.hercules-ci-effects.flakeModule
  ];
  hercules-ci.github-pages.branch = "alberto/serve-docs";
  perSystem = { config, ... }: {
    hercules-ci.github-pages.settings.contents = config.packages.ytxp-lib-96-docs + "/share/doc";
  };
}
