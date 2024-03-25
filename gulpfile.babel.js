// Generated on 2016-03-13 using generator-angular-fullstack 3.4.2
'use strict';

import _ from 'lodash';
import del from 'del';
import gulp from 'gulp';
import path from 'path';
import gulpLoadPlugins from 'gulp-load-plugins';
import http from 'http';
import open from 'open';
import {stream as wiredep} from 'wiredep';
import {Server as KarmaServer} from 'karma';
import runSequence from 'run-sequence';
import {protractor, webdriver_update} from 'gulp-protractor';
import Server from "aliv";
import nodemon from 'nodemon';

var plugins = gulpLoadPlugins();
var config = { port: 9000 };

const serverPath = 'dev-server';
const clientPath = require('./bower.json').appPath || 'client';
const paths = {
    client: {
        assets: `${clientPath}/assets/**/*`,
        images: `${clientPath}/assets/images/**/*`,
        scripts: [
            `${clientPath}/**/!(*.spec|*.mock).js`,
            `!${clientPath}/bower_components/**/*`,
	        `!${clientPath}/assets/help/**/*`
        ],
        styles: [`${clientPath}/{app,components}/**/*.scss`],
        mainStyle: `${clientPath}/app/app.scss`,
        views: `${clientPath}/{app,components}/**/*.html`,
        mainView: `${clientPath}/index.html`,
        test: [`${clientPath}/{app,components}/**/*.{spec,mock}.js`],
        e2e: ['e2e/**/*.spec.js'],
        bower: `${clientPath}/bower_components/`
    },
    dist: 'dist'
};

/********************
 * Helper functions
 ********************/

var debug = require('gulp-debug');
var git = require('git-rev-sync');
var replace = require('gulp-string-replace');

function onServerLog(log) {
    console.log(plugins.util.colors.white('[') +
        plugins.util.colors.yellow('nodemon') +
        plugins.util.colors.white('] ') +
        log.message);
}

function checkAppReady(cb) {
    var options = {
        host: '127.0.0.1',
        port: config.port
    };
	http
		.get(options, () => cb(true))
		.on('error', () => cb(false));
}

// Call page until first success
function whenServerReady(cb) {
	// let aliv = new Server({
	// 	watch: false,
	// 	root: process.cwd()
	// });
	//
	// aliv.start();

	var serverReady = false;
    var appReadyInterval = setInterval(() =>
        checkAppReady((ready) => {
            if (!ready || serverReady) {
                return;
            }
            clearInterval(appReadyInterval);
            serverReady = true;
            cb();
        }),
        100);
}

function sortModulesFirst(a, b) {
    var module = /\.module\.js$/;
    var aMod = module.test(a.path);
    var bMod = module.test(b.path);
    // inject *.module.js first
    if (aMod === bMod) {
        // either both modules or both non-modules, so just sort normally
        if (a.path < b.path) {
            return -1;
        }
        if (a.path > b.path) {
            return 1;
        }
        return 0;
    } else {
        return (aMod ? -1 : 1);
    }
}

/********************
 * Tasks
 ********************/

gulp.task('inject', cb => {
    runSequence(['inject:js', 'inject:css', 'inject:scss'], cb);
});

gulp.task('inject:js', () => {
    return gulp.src(paths.client.mainView)
        .pipe(plugins.inject(
            gulp.src(_.union(paths.client.scripts, [`!${clientPath}/**/*.{spec,mock}.js`, `!${clientPath}/app/app.js`]), {read: false})
                .pipe(plugins.sort(sortModulesFirst)),
            {
                starttag: '<!-- injector:js -->',
                endtag: '<!-- endinjector -->',
                transform: (filepath) => '<script src="' + filepath.replace(`/${clientPath}/`, '') + '"></script>'
            }))
        .pipe(gulp.dest(clientPath));
});

gulp.task('inject:css', () => {
    return gulp.src(paths.client.mainView)
        .pipe(plugins.inject(
            gulp.src(`/${clientPath}/{app,components}/**/*.css`, {read: false})
                .pipe(plugins.sort()),
            {
                starttag: '<!-- injector:css -->',
                endtag: '<!-- endinjector -->',
                transform: (filepath) => '<link rel="stylesheet" href="' + filepath.replace(`/${clientPath}/`, '').replace('/.tmp/', '') + '">'
            }))
        .pipe(gulp.dest(clientPath));
});

gulp.task('inject:scss', () => {
	return gulp.src(paths.client.mainStyle)
		.pipe(plugins.inject(
			gulp.src(_.union(paths.client.styles, ['!' + paths.client.mainStyle]), {read: false})
				.pipe(plugins.sort()),
			{
				starttag: '// injector',
				endtag: '// endinjector',
				transform: (filepath) => {
					let newPath = filepath
						.replace(`/${clientPath}/app/`, '')
						.replace(`/${clientPath}/components/`, '../components/')
						.replace(/_(.*).scss/, (match, p1, offset, string) => p1)
						.replace('.scss', '');
					return `@import '${newPath}';`;
				}
			}))
		.pipe(gulp.dest(`${clientPath}/app`));
});


gulp.task('styles', () => {
    return gulp.src(paths.client.mainStyle)
		.pipe(plugins.sourcemaps.init())
		.pipe(plugins.sass())
		.pipe(plugins.autoprefixer({browsers: ['last 1 version']}))
		.pipe(plugins.sourcemaps.write('.'))
		.pipe(gulp.dest('.tmp/app'));
});

gulp.task('transpile:client', () => {
    return gulp.src(paths.client.scripts)
		.pipe(plugins.sourcemaps.init())
		.pipe(plugins.babel({
			plugins: ["transform-class-properties"]
		}))
		.pipe(plugins.sourcemaps.write('.'))
		.pipe(gulp.dest('.tmp'));
});

gulp.task('lint:scripts', cb => runSequence(['lint:scripts:client'], cb));

gulp.task('lint:scripts:client', () => {
    return gulp.src(_.union(
        paths.client.scripts,
        _.map(paths.client.test, blob => '!' + blob),
        [`!${clientPath}/app/app.constant.js`]
    ))
		.pipe(plugins.jshint(`.jshintrc`))
		.pipe(plugins.jshint.reporter('jshint-stylish'));
});

gulp.task('lint:scripts:clientTest', () => {
    return gulp.src(paths.client.test)
		.pipe(plugins.jshint(`.jshintrc`))
		.pipe(plugins.jshint.reporter('jshint-stylish'));
});

gulp.task('jscs', () => {
  return gulp.src(_.union(paths.client.scripts))
      .pipe(plugins.jscs())
      .pipe(plugins.jscs.reporter());
});

gulp.task('clean:tmp', () => del(['.tmp/**/*'], {dot: true}));

gulp.task('start:client', cb => {
    whenServerReady(() => {
        open('http://localhost:' + config.port);
        cb();
    });
});

gulp.task('start:server', () => {
	process.env.NODE_ENV = 'development';
	nodemon(`-w ${serverPath} ${serverPath}`)
		.on('log', onServerLog);
});


gulp.task('watch', () => {
    var testFiles = _.union(paths.client.test);

    plugins.livereload.listen();

    plugins.watch(paths.client.styles, () => {  //['inject:scss']
        gulp.src(paths.client.mainStyle)
            .pipe(plugins.plumber())
			.pipe(plugins.sourcemaps.init())
			.pipe(plugins.sass())
			.pipe(plugins.autoprefixer({browsers: ['last 1 version']}))
			.pipe(plugins.sourcemaps.write('.'))
			.pipe(gulp.dest('.tmp/app'))
            .pipe(plugins.livereload());
    });

    plugins.watch(paths.client.views)
        .pipe(plugins.plumber())
        .pipe(plugins.livereload());

    plugins.watch(paths.client.scripts) //['inject:js']
        .pipe(plugins.plumber())
		.pipe(plugins.sourcemaps.init())
		.pipe(plugins.babel({
			plugins: ["transform-class-properties"]
		}))
		.pipe(plugins.sourcemaps.write('.'))
        .pipe(gulp.dest('.tmp'))
        .pipe(plugins.livereload());

    gulp.watch('bower.json', ['wiredep:client']);
});

gulp.task('serve', cb => {
    runSequence(['clean:tmp'],
        ['lint:scripts', 'inject'],
        ['wiredep:client'],
        ['transpile:client', 'styles'],
        ['start:server'],
        'watch',
        cb);
});

gulp.task('serve:dist', cb => {
    runSequence(
        'build',
        'env:all',
        'env:prod',
        cb);
});

gulp.task('test', cb => {
    return runSequence('test:client', cb);
});

gulp.task('test:client', ['wiredep:test'], (done) => {
    new KarmaServer({
      configFile: `${__dirname}/${paths.karma}`,
      singleRun: true
    }, done).start();
});

// inject bower components
gulp.task('wiredep:client', () => {
    return gulp.src(paths.client.mainView)
        .pipe(wiredep({
            exclude: [
                /bootstrap-sass-official/,
                /bootstrap.js/,
                /json3/,
                /es5-shim/,
                /bootstrap.css/,
                /font-awesome.css/
            ],
            ignorePath: clientPath
        }))
        .pipe(gulp.dest(`${clientPath}/`));
});

gulp.task('wiredep:test', () => {
    return gulp.src(paths.karma)
        .pipe(wiredep({
            exclude: [
                /bootstrap-sass-official/,
                /bootstrap.js/,
                '/json3/',
                '/es5-shim/',
                /bootstrap.css/,
                /font-awesome.css/
            ],
            devDependencies: true
        }))
        .pipe(gulp.dest('./'));
});

/********************
 * Build
 ********************/

//FIXME: looks like font-awesome isn't getting loaded
gulp.task('build', cb => {
    runSequence(
        [
            'clean:dist',
            'clean:tmp'
        ],
        'inject',
        'wiredep:client',
        [
            'build:images',
            'copy:extras',
            'copy:fonts',
            'copy:assets',
            'build:client'
        ],
        cb);
});

gulp.task('clean:dist', () => del([`${paths.dist}/client/!(.git*|.openshift|Procfile)**`], {dot: true}));

gulp.task('build:client', ['transpile:client', 'styles', 'html'], () => {
    var manifest = gulp.src(`${paths.dist}/${clientPath}/assets/rev-manifest.json`);
    var appFilter = plugins.filter('**/app.js', { restore: true });
    var jsFilter = plugins.filter('**/*.js', { restore: true });
    var cssFilter = plugins.filter('**/*.css', { restore: true });
    var htmlBlock = plugins.filter(['**/*.!(html)'], { restore: true });

    return gulp.src(paths.client.mainView)
        .pipe(plugins.useref())
            .pipe(appFilter)
                .pipe(plugins.addSrc.append('.tmp/templates.js'))
                .pipe(plugins.concat('app/app.js'))
            .pipe(appFilter.restore)
            .pipe(jsFilter)
                .pipe(plugins.ngAnnotate())
				.pipe(plugins.uglify({
					mangle: false
				}))
            .pipe(jsFilter.restore)
            .pipe(cssFilter)
                .pipe(plugins.minifyCss({
                    cache: true,
                    processImportFrom: ['!fonts.googleapis.com']
                }))
            .pipe(cssFilter.restore)
            .pipe(htmlBlock)
                .pipe(plugins.rev())
            .pipe(htmlBlock.restore)
        .pipe(plugins.revReplace({manifest}))
		.pipe(replace('{{git}}', git.long()))
        .pipe(gulp.dest(`${paths.dist}/${clientPath}`))
});

gulp.task('html', function() {
    return gulp.src(`${clientPath}/{app,components}/**/*.html`)
        .pipe(plugins.angularTemplatecache({
            module: 'gigaApp'
        }))
        .pipe(gulp.dest('.tmp'));
});

gulp.task('build:images', () => {
    return gulp.src(paths.client.images)
      //  .pipe(plugins.imagemin({
      //      optimizationLevel: 5,
      //      progressive: true,
      //      interlaced: true
      //  }))
        //.pipe(plugins.rev())
        .pipe(gulp.dest(`${paths.dist}/${clientPath}/assets/images`))
        .pipe(plugins.rev.manifest(`${paths.dist}/${clientPath}/assets/rev-manifest.json`, {
            base: `${paths.dist}/${clientPath}/assets`,
            merge: true
        }))
        .pipe(gulp.dest(`${paths.dist}/${clientPath}/assets`));
});

gulp.task('copy:extras', () => {
    return gulp.src([
        `${clientPath}/favicon.ico`,
        `${clientPath}/robots.txt`,
        `${clientPath}/.htaccess`
    ], { dot: true })
        .pipe(gulp.dest(`${paths.dist}/${clientPath}`));
});

gulp.task('copy:fonts', () => {
    return gulp.src(`${clientPath}/bower_components/{bootstrap,font-awesome}/fonts/**/*`, { dot: true })
        .pipe(gulp.dest(`${paths.dist}/${clientPath}/bower_components`));
});

gulp.task('copy:assets', () => {
    return gulp.src([paths.client.assets, '!' + paths.client.images])
        .pipe(gulp.dest(`${paths.dist}/${clientPath}/assets`));
});

// Downloads the selenium webdriver
gulp.task('webdriver_update', webdriver_update);

gulp.task('test:e2e', ['env:all', 'env:test', 'webdriver_update'], cb => {
    gulp.src(paths.client.e2e)
        .pipe(protractor({
            configFile: 'protractor.conf.js',
        })).on('error', err => {
            console.log(err)
        }).on('end', () => {
            process.exit();
        });
});