module.exports = function (grunt) {
  'use strict';

  // set up common objects for jslint
  var jsLintStandardOptions = { edition: 'latest', errorsOnly: true, failOnError: true };

  var serverDirectives = function () {
    return { indent: 2, node: true, nomen: true, todo: true, unparam: true, vars: true };
  };
  var jsLintServerDirectives = serverDirectives();
  var jsLintServerTestDirectives = serverDirectives();
  jsLintServerTestDirectives.ass = true;
  jsLintServerTestDirectives.predef = ['afterEach', 'after', 'beforeEach', 'before', 'describe', 'it'];

  var commonJSfiles = [
    'node_modules/jquery/dist/jquery.js',
    'node_modules/bootstrap/dist/js/bootstrap.js'
  ];

  // filesets for uglify
  var files_de = {
    'public/clientscripts/global_de.js': commonJSfiles.concat([
      'node_modules/jquery-validation/src/localization/messages_de.js',
      'node_modules/jquery-validation/src/localization/methods_de.js',
      'node_modules/bootstrap-datepicker/js/locales/bootstrap-datepicker.de.js',
      'node_modules/select2/select2_locale_de.js',
      'node_modules/fullcalendar/dist/lang/de.js',
      'locales/frontend_de.js',
      'frontend/javascript/agora.js'
    ])
  };

  grunt.initConfig({
    clean: ['bower_components', 'css', 'fonts', 'js'],
    copy: {
      bootstrapFONTS: {
        src: 'node_modules/bootstrap/dist/fonts/*',
        dest: '../jekyll/fonts',
        expand: true,
        flatten: true
      },
      bootstrapCSS: {
        src: 'node_modules/bootstrap/dist/css/bootstrap.min.css',
        dest: '../jekyll/css',
        expand: true,
        flatten: true
      },
      bootstrapJS: {
        src: 'node_modules/bootstrap/dist/js/bootstrap.min.js',
        dest: '../jekyll/js',
        expand: true,
        flatten: true
      },
      fontawesomeFONTS: {
        src: 'node_modules/font-awesome/fonts/*',
        dest: '../jekyll/fonts',
        expand: true,
        flatten: true
      }
    }
  });

  // These plugins provide necessary tasks.
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-contrib-less');

  // Default task.
  grunt.registerTask('default', ['copy']);

  grunt.registerTask('delete', ['clean']);

};
