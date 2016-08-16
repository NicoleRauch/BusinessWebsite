module.exports = function (grunt) {
  /*eslint camelcase: 0*/
  'use strict';

  grunt.initConfig({
    less: {
      minify: {
        options: {
          cleancss: true,
          report: 'min'
        },
        files: {
          'hakyll/css/screen.css': [
            'build/stylesheets/less/custom.less'
          ]
        }
      }
    }
  });

  // These plugins provide necessary tasks.
  grunt.loadNpmTasks('grunt-contrib-less');

  // Default task.
  grunt.registerTask('default', ['less']);
};
