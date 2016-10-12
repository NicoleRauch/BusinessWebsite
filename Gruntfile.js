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
    },

    pkg: grunt.file.readJSON('package.json'),

    useminPrepare: {
      html: 'HTML/index.html'
    },

    usemin: {
      html: ['HTML/index.html']
    },

    critical: {
      build: {
        options: {
          base: 'HTML/',
          width: 260,
          height: 640
        },
        src: 'HTML/index.html',
        dest: 'HTML/index.html'
      }
    },

  });

  // These plugins provide necessary tasks.
  //grunt.loadNpmTasks('grunt-contrib-less');

//  grunt.loadNpmTasks('grunt-usemin');

  // Default task.
  // Load all files starting with `grunt-`
  require('matchdep').filter('grunt-*').forEach(grunt.loadNpmTasks);

  grunt.registerTask('default', ['less']);
  grunt.registerTask('usemin-optimize', ['useminPrepare', 'concat:generated', 'cssmin:generated', 'uglify:generated', 'usemin']);
  grunt.registerTask('optimize', ['usemin-optimize']);
  grunt.registerTask('css-mangling', [
      // 'uncss:build', 'cssmin:build',
    'critical:build']);

};
