/*eslint no-undef: 0, quotes: 0 */
var gulp = require('gulp'),
    purescript = require('gulp-purescript');

var purescriptMakeOptions = {
  src: [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs"
  ],
  ffi: [
    "src/purescript/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
  ],
  output: "transpiled/purescript/output"
};

var purescriptBundleOptions = {
  src: purescriptMakeOptions.output + "/**/*.js",
  output: "develop/JS/bundle.js"
};

gulp.task("make", function () {
  return purescript.psc(purescriptMakeOptions);
});

gulp.task("bundle", ["make"], function () {
  return purescript.pscBundle(purescriptBundleOptions);
});

gulp.task("dotpsci", ["bundle"], function () {
  return purescript.psci(purescriptMakeOptions)
    .pipe(gulp.dest("."));
});

//keep an eye on what is changing.
gulp.task('enable-live-changes', function() {
  gulp.watch(purescriptMakeOptions.src, ['dotpsci']);
});

gulp.task('default', ['dotpsci']);
