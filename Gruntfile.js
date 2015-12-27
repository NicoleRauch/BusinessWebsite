module.exports = function (grunt) {
  /*eslint camelcase: 0*/
  'use strict';

  var commonJSfiles = [
    'node_modules/jquery/dist/jquery.js',
    'node_modules/bootstrap/dist/js/bootstrap.js'
  ];

  // filesets for uglify
  var files_de = {
    'public/clientscripts/global_de.js': commonJSfiles.concat([
      'frontend/javascript/agora.js'
    ])
  };

  var files_en = {
    'public/clientscripts/global_en.js': commonJSfiles.concat([
      'frontend/javascript/agora.js'
    ])
  };

  grunt.initConfig({
    clean: {
      build: ['build', 'frontendtests/fixtures/*.html'],
      public: ['public/clientscripts', 'public/fonts', 'public/img/bootstrap-colorpicker', 'public/images', 'public/stylesheets'],
      options: {force: true}
    },
    copy: {
      bootstrapFONTS: {
        src: 'node_modules/bootstrap/dist/fonts/*',
        dest: 'jekyll/fonts',
        expand: true,
        flatten: true
      },
      fontawesomeFONTS: {
        src: 'node_modules/font-awesome/fonts/*',
        dest: 'jekyll/fonts',
        expand: true,
        flatten: true
      },
      bootstrapLESS: {
        cwd: 'node_modules/bootstrap/less/',
        src: ['**', '!variables.less'],
        dest: 'build/stylesheets/less',
        expand: true,
        flatten: false
      },
      bootstrapCustomVariablesLESS: {
        src: 'node_modules/bootstrap/less/variables.less',
        dest: 'build/stylesheets/less/original-variables.less'
      },
      customLESS: {
        src: 'frontend/less/*',
        dest: 'build/stylesheets/less',
        expand: true,
        flatten: true
      },
      fullpageCSS: {
        src: 'node_modules/fullpage.js/jquery.fullPage.css',
        dest: 'jekyll/css',
        expand: true,
        flatten: true
      },
      jqueryJS: {
        src: 'node_modules/jquery/dist/jquery.min.js',
        dest: 'jekyll/js',
        expand: true,
        flatten: true
      },
      bootstrapJS: {
        src: 'node_modules/bootstrap/dist/js/bootstrap.min.js',
        dest: 'jekyll/js',
        expand: true,
        flatten: true
      },
      jqueryEasingJS: {
        src: 'node_modules/jquery.easing/jquery.easing.min.js',
        dest: 'jekyll/js',
        expand: true,
        flatten: true
      },
      fullpageJS: {
        src: 'node_modules/fullpage.js/jquery.fullPage.js',
        dest: 'jekyll/js',
        expand: true,
        flatten: true
      }
      /*
      ,
      customJS: {
        cwd: 'frontend/javascript/',
        src: ['*', '!agora.js'],
        dest: 'public/clientscripts',
        expand: true,
        flatten: false
      }
      */
    },
    eslint: {
      options: {quiet: true},
      target: ['**/*.js']
    },
    karma: {
      options: {
        configFile: 'karma.conf.js'
      },
      once: {
        browsers: ['PhantomJS'],
        runnerPort: 6666,
        singleRun: true
      }
    },
    less: {
      minify: {
        options: {
          cleancss: true,
          report: 'min'
        },
        files: {
          'jekyll/css/screen.css': [
            'build/stylesheets/less/custom.less'
          ]
        }
      }
    },
    uglify: {
      development_de: {
        options: {
          mangle: false,
          beautify: true
        },
        files: files_de
      },
      development_en: {
        options: {beautify: true},
        files: files_en
      },
      production_de: {
        files: files_de
      },
      production_en: {
        files: files_en
      }
    }
  });

  // These plugins provide necessary tasks.
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-copy');
  grunt.loadNpmTasks('grunt-contrib-jade');
  grunt.loadNpmTasks('grunt-contrib-less');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-eslint');

  grunt.registerTask('prepare', ['copy', 'less']);
  grunt.registerTask('frontendtests', ['clean', 'prepare', 'jade', 'uglify:production_de', 'karma:once', 'uglify:development_de', 'karma:once', 'istanbul_check_coverage:frontend']);
  grunt.registerTask('deploy_development', ['prepare', 'uglify:development_de', 'uglify:development_en']);

  // Default task.
  grunt.registerTask('default', ['tests', 'uglify:development_en']);

  grunt.registerTask('deploy_production', ['clean', 'prepare', 'uglify:production_de', 'uglify:production_en']);
};
