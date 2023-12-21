'use strict';

(function () {

	class AdminProjectSetupComponent {
		//start-non-standard
		model = {
			params: {}
		};

		project = {};

		//end-non-standard

		constructor($scope, $state, Modal, $http, Util, $translate, Upload, ProjectsResource, $location, leafletBoundsHelpers, leafletData, appConfig, Auth) {
			this.translations = {};
			$scope.isSysadmin = Auth.hasRole('sysadmin');
			$scope.isCountriesMode = appConfig.isCountriesMode;
			$scope.isSchoolsMode = appConfig.isSchoolsMode;
			$scope.isGlobalMode = appConfig.isGlobalMode;
			this.scope = $scope;
			this.state = $state;
			this.http = $http;
			this.modal = Modal;
			this.upload = Upload;
			this.project = $scope.$parent.$resolve.loadProject.data;
			this.results_summary = $scope.$parent.$resolve.loadSummary.data;
			this.countries = $scope.$parent.$resolve.loadCountries.data;
			if ($scope.isSchoolsMode) {
				this.input_template = $scope.$parent.$resolve.loadTemplateInfo.data;
			} else {
				this.input_template = {};
			}
			this.variables = $scope.$parent.$resolve.loadVariables.data;
			this.topology = $scope.$parent.$resolve.loadTopology.data;
			this.topology_summary = $scope.$parent.$resolve.loadTopologySummary.data;
			if (appConfig.isSchoolsMode) {
				this.prepareSchoolsProject($scope, $state, Modal, $http, Util, $translate, Upload, ProjectsResource, $location, leafletBoundsHelpers, leafletData);
			} else if (appConfig.isCountriesMode) {
				this.prepareCountriesProject($scope, $state, Modal, $http, Util, $translate, Upload, ProjectsResource, $location);
			} else {
				this.prepareGlobalProject($scope, $state, Modal, $http, Util, $translate, Upload, ProjectsResource, $location);
			}
			//Group variables by var_class
			this.rebuildVariables();
			//Unpack project settings
			this.settings = JSON.parse(this.project.settings);
			this.validation_data = this.project.validationdata ? JSON.parse(this.project.validationdata) : false;
			this.status_data = this.project.statusdata ? JSON.parse(this.project.statusdata) : false;
			this.ProjectsResource = ProjectsResource;
			$scope.scrollTo = function (hash, $event) {
				$location.hash(hash);
			}
			this.scope.$on('$stateChangeStart', (event, toState, toParams, fromState, fromParams) => {
				if (fromState.name === 'admin-project-setup' && typeof this.reloadCalculationsProgress.timeout !== 'undefined') {
					clearTimeout(this.reloadCalculationsProgress.timeout);
				}
			})
			//Init progress reload
			if (['waiting', 'in-progress','stop'].includes(this.project.status)) {
				this.reloadCalculationsProgress();
			}
			this.confirmDelete = Modal.confirm.delete((project) => {
				this.deleteProject(project);
			});
			this.confirmRestoreDefaultVariables = Modal.confirm.simple((project) => {
				this.restoreDefaultVariables(project);
			});

			//Init topology graph
			if ( $scope.isGlobalMode || $scope.isCountriesMode || ($scope.isSchoolsMode && this.settings.is_net_in_output === true) && typeof this.topology.markers !== 'undefined' && typeof this.topology.markers['m0'] !== 'undefined' ) {
				this.scope.show_topology = true;
				this.leafletBoundsHelpers = leafletBoundsHelpers;
				this.topology_events = {
					markers: {
						enable: ['mouseover', 'mouseout'],
					}
				};
				$scope.$on("leafletDirectiveMarker.topology.mouseover", (event, args) => {
					args.leafletObject.label._container.style.zIndex = 99999999;
					args.leafletObject.label._container.innerHTML = '<b>' + args.leafletObject.label._content + '</b>';
					if (typeof this.topology.markers_message[args.leafletObject.label._content] !== 'undefined') {
						args.leafletObject.label._container.innerHTML += '<br/>' + this.topology.markers_message[args.leafletObject.label._content]
					}

				});
				$scope.$on("leafletDirectiveMarker.topology.mouseout", (event, args) => {
					args.leafletObject.label._container.innerHTML = args.leafletObject.label._content;
					args.leafletObject.label._container.style.zIndex = args.leafletObject.label._zIndex;
				});
				this.graph();
				setTimeout(() => {
					leafletData.getMap('topology').then(function (map) {
						L.easyPrint({
							sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
							exportOnly: true,
							hideControlContainer: true
						}).addTo(map);
					});
				});
				this.scope.refreshTopology = () => {
					let ctrl = this;
					setTimeout(()=>{
						leafletData.getMap('topology').then(function (map) {
							//TODO: figure out how to update bounds, map.fitBounds();
							map.invalidateSize(false);
							map.setZoom(9);
						});
					});
				};
			}
		}

		prepareSchoolsProject($scope, $state, Modal, $http, Util, $translate, Upload, ProjectsResource, $location, leafletBoundsHelpers, leafletData) {
			$translate([
				'admin-results-overview.dia_cel',
				'admin-results-overview.dia_sat',
				'admin-results-overview.dia_mw',
				'admin-results-overview.dia_focl',
				'admin-results-overview.dia_title_count_costofownership5',
				'admin-results-overview.dia_subtitle_count_costofownership5',
				'admin-results-overview.dia_title_count_costofownership10',
				'admin-results-overview.dia_subtitle_count_costofownership10',
				'admin-results-overview.dia_title_count_costofownership15',
				'admin-results-overview.dia_subtitle_count_costofownership15',
				'admin-results-overview.dia_title_count_costofownership20',
				'admin-results-overview.dia_subtitle_count_costofownership20',
				'admin-results-overview.dia_title_count_npv5',
				'admin-results-overview.dia_subtitle_count_npv5',
				'admin-results-overview.dia_title_count_npv10',
				'admin-results-overview.dia_subtitle_count_npv10',
				'admin-results-overview.dia_title_count_npv15',
				'admin-results-overview.dia_subtitle_count_npv15',
				'admin-results-overview.dia_title_count_npv20',
				'admin-results-overview.dia_subtitle_count_npv20',
				'admin-results-overview.dia_title_money_capex5',
				'admin-results-overview.dia_subtitle_money_capex5',
				'admin-results-overview.dia_title_money_capex20',
				'admin-results-overview.dia_subtitle_money_capex20',
				'admin-results-overview.dia_title_money_opex5',
				'admin-results-overview.dia_subtitle_money_opex5',
				'admin-results-overview.dia_title_money_opex20',
				'admin-results-overview.dia_subtitle_money_opex20',
				'admin-results-overview.dia_title_money_costofown5',
				'admin-results-overview.dia_subtitle_money_costofown5',
				'admin-results-overview.dia_title_money_costofown20',
				'admin-results-overview.dia_subtitle_money_costofown20',
				'admin-results-overview.dia_title_income_costofown5',
				'admin-results-overview.dia_subtitle_income_costofown5',
				'admin-results-overview.dia_title_income_costofown20',
				'admin-results-overview.dia_subtitle_income_costofown20'
			]).then((result) => {
				this.translations = result;
				if (typeof this.results_summary.diagram !== 'undefined' && !Array.isArray(this.results_summary.diagram)) {
					this.prepareResultsSummaryDiagrams();
				}
			});
		}

		prepareCountriesProject($scope, $state, Modal, $http, Util, $translate, Upload, ProjectsResource, $location) {

		}

		prepareGlobalProject($scope, $state, Modal, $http, Util, $translate, Upload, ProjectsResource, $location) {

		}

		deleteProject(project) {
			this.ProjectsResource.delete({id:project.id}, ()=>{
				this.state.go('main');
			});
		}

		prepareResultsSummaryDiagrams() {
			let translateSeries = (part, index, series) => {
				if (typeof this.translations['admin-results-overview.' + part.text] !== 'undefined') {
					part.text = this.translations['admin-results-overview.' + part.text];
				}
				return part;
			};

			this.results_diagrams = [];
			let default_legend = {};
			//count_costofownership
			[5, 10, 15, 20].forEach((num) => {
				if (typeof this.results_summary.diagram['count_costofownership' + num] === 'undefined') {
					return;
				}
				this.results_diagrams['count_costofownership' + num] = {
					type: "pie",
					legend: {y: "10%"},
					title: {
						'font-size': 18,
						textAlign: 'center',
						text: this.translations['admin-results-overview.dia_title_count_costofownership' + num]
					},
					subtitle: {
						'font-size': 14,
						text: this.translations['admin-results-overview.dia_subtitle_count_costofownership' + num]
					},
					series: this.results_summary.diagram['count_costofownership' + num].map(translateSeries)
				};
			});

			//count_npv
			[5, 10, 15, 20].forEach((num) => {
				if (typeof this.results_summary.diagram['count_npv' + num] === 'undefined') {
					return;
				}
				this.results_diagrams['count_npv' + num] = {
					type: "pie",
					legend: {y: "10%"},
					title: {
						'font-size': 18,
						textAlign: 'center',
						text: this.translations['admin-results-overview.dia_title_count_npv' + num]
					},
					subtitle: {
						'font-size': 14,
						text: this.translations['admin-results-overview.dia_subtitle_count_npv' + num]
					},
					series: this.results_summary.diagram['count_npv' + num].map(translateSeries)
				};
			});

			//money_capex
			[5, 20].forEach((num) => {
				if (typeof this.results_summary.diagram['money_capex' + num] === 'undefined') {
					return;
				}

				this.results_diagrams['money_capex' + num] = {
					type: "pie",
					legend: {y: "10%"},
					title: {
						'font-size': 18,
						textAlign: 'center',
						text: this.translations['admin-results-overview.dia_title_money_capex' + num]
					},
					subtitle: {
						'font-size': 14,
						text: this.translations['admin-results-overview.dia_subtitle_money_capex' + num]
					},
					series: this.results_summary.diagram['money_capex' + num].map(translateSeries)
				};
			});

			//money_opex
			[5, 20].forEach((num) => {
				if (typeof this.results_summary.diagram['money_opex' + num] === 'undefined') {
					return;
				}
				this.results_diagrams['money_opex' + num] = {
					type: "pie",
					legend: {y: "10%"},
					title: {
						'font-size': 18,
						textAlign: 'center',
						text: this.translations['admin-results-overview.dia_title_money_opex' + num]
					},
					subtitle: {
						'font-size': 14,
						text: this.translations['admin-results-overview.dia_subtitle_money_opex' + num]
					},
					series: this.results_summary.diagram['money_opex' + num].map(translateSeries)
				};
			});

			//money_costofown
			[5, 20].forEach((num) => {
				if (typeof this.results_summary.diagram['money_costofown' + num] === 'undefined') {
					return;
				}
				this.results_diagrams['money_costofown' + num] = {
					type: "pie",
					legend: {y: "10%"},
					title: {
						'font-size': 18,
						textAlign: 'center',
						text: this.translations['admin-results-overview.dia_title_money_costofown' + num]
					},
					subtitle: {
						'font-size': 14,
						text: this.translations['admin-results-overview.dia_subtitle_money_costofown' + num]
					},
					series: this.results_summary.diagram['money_costofown' + num].map(translateSeries)
				};
			});
			//income_costofown
			[5, 20].forEach((num) => {
				if (typeof this.results_summary.diagram['income_costofown' + num] === 'undefined') {
					return;
				}
				this.results_diagrams['income_costofown' + num] = {
					type: "pie",
					legend: {y: "10%"},
					title: {
						'font-size': 18,
						textAlign: 'center',
						text: this.translations['admin-results-overview.dia_title_income_costofown' + num]
					},
					subtitle: {
						'font-size': 14,
						text: this.translations['admin-results-overview.dia_subtitle_income_costofown' + num]
					},
					series: this.results_summary.diagram['income_costofown' + num].map(translateSeries)
				};
			});
		}

		graph() {
			this.topology_watch_options = {
				paths: {
					individual: {type: 'watch'}, //this keeps infdigest errors from happening.... (deep by default)
					type: 'watchCollection'
				}
			};
			this.topology_controls = {
				fullscreen: {
					position: 'topleft'
				}
			};
			var local_icons = {
				default_icon: {},
				home:  {
					type: 'awesomeMarker',
						icon: 'home',
						markerColor: 'purple'
				}
			};
			angular.extend(this.scope, {
				icons: local_icons
			});
			angular.forEach(this.topology.markers, (obj, name) => {
				if (obj.type === 'home') {
					//console.log(this.topology.markers[name].icon);
					obj.icon = local_icons.home;
				}
			});
			this.topology_maxbounds = this.leafletBoundsHelpers.createBoundsFromArray(this.topology.maxbounds);
			this.topology_paths = this.topology.paths;
			this.topology_markers = this.topology.markers;
			this.topology_defaults = {
				scrollWheelZoom: true
			};
		}

		calculate(project) {
			this.ProjectsResource.calculate({id: project.id}, () => {
				this.state.reload();
			});
		}

		stop(project) {
			this.ProjectsResource.stop({id: project.id}, () => {
				this.state.reload();
			});
		}

		calculateTopology(project) {
			this.ProjectsResource.calculateTopology({id: project.id}, () => {
				this.state.reload();
			});
		}

		reloadCalculationsProgress() {
			console.log(this.project.status, this.project.progress);
			this.reloadCalculationsProgress.timeout = setTimeout(() => {
				this.http({method: 'GET', url: '/api/projects/' + this.project.id + '/progress'}).then((response) => {
					this.project.status = response.data.status;
					this.project.progress = response.data.progress;
					if (!(['waiting', 'in-progress','stop'].includes(response.data.status))) {
						this.state.reload();
						return;
					}
					this.reloadCalculationsProgress();
				});
			}, 5000);
		}

		downloadXLSX(project) {
			this.ProjectsResource.downloadXLSX({id: project.id});
		}

		reloadVariables() {
			this.http({method: 'GET', url: '/api/variables/project/' + this.project.id}).then((response) => {
				this.variables = response.data;
				this.rebuildVariables();
			});
		}

		rebuildVariables() {
			let vars = {};
			this.variables.forEach((v) => {
				if (typeof vars[v.var_class] === 'undefined') vars[v.var_class] = {};
				if (typeof vars[v.var_class][v.technology] === 'undefined') vars[v.var_class][v.technology] = [];
				vars[v.var_class][v.technology].push(v);
			});
			this.variables = vars;
		}

		restoreDefaultVariables() {
			this.http({method: 'DELETE', url: '/api/variables/reset/project/' + this.project.id}).then((response) => {
				if (response.data.status === 'success') {
					this.state.reload();
					return;
				}
			});
		}

		uploadVarsFile(file, errFiles) {
			this.vars_file = file;
			this.vars_errFile = errFiles && errFiles[0];
			if (file) {
				file.upload = this.upload.upload({
					url: '/api/variables/project/' + this.project.id + '/xlsx',
					data: {file: file},
					method: 'POST'
				});

				file.upload.then((response) => {
					setTimeout(() => {
						file.result = response.data;
					});
				}, (response) => {
					if (response.status > 0)
						this.vars_errorMsg = response.status + ': ' + response.data;
				}, (evt) => {
					file.progress = Math.min(100, parseInt(100.0 *
						evt.loaded / evt.total));
					if (file.progress === 100) {
						if (this.uploadVarsFile.timeout) {
							clearTimeout(this.uploadVarsFile.timeout);
						}
						this.uploadVarsFile.timeout = setTimeout(() => {
							this.uploadVarsFile.timeout = null;
							console.log(file.progress);
							this.reloadVariables();
						}, 1000);
					}
				});
			}
		}

		uploadInputFile(file, errFiles) {
			this.file = file;
			this.errFile = errFiles && errFiles[0];
			if (file) {
				file.upload = this.upload.upload({
					url: '/api/projects/' + this.project.id + '/files',
					data: {file: file},
					method: 'POST'
				});

				file.upload.then((response) => {
					setTimeout(() => {
						file.result = response.data;
					});
				}, (response) => {
					if (response.status > 0)
						this.errorMsg = response.status + ': ' + response.data;
				}, (evt) => {
					file.progress = Math.min(100, parseInt(100.0 *
						evt.loaded / evt.total));
					if (file.progress === 100) {
						if (this.uploadInputFile.timeout) {
							clearTimeout(this.uploadInputFile.timeout);
						}
						this.uploadInputFile.timeout = setTimeout(() => {
							this.uploadInputFile.timeout = null;
							this.state.reload();
						}, 1000);
					}
				});
			}
		}

		searchReload() {
			let old = this.pagination.objects.currentPage;
			this.pagination.objects.currentPage = 1;
			if (old === 1) {
				this.objectsReload(1);
			}
		}

		freshReload() {
			let old = this.pagination.objects.currentPage;
			this.pagination.objects.filterByName = '';
			this.pagination.objects.currentPage = 1;
			if (old === 1) {
				this.objectsReload(1);
			}
		}

		objectsReload(newValue, oldValue) {
			if (newValue === oldValue) return;//called on initialization
			let limit = this.pagination.objects.itemsPerPage;
			let offset = (this.pagination.objects.currentPage - 1) * limit;
			let url = '/api/objects/project/' + this.project.id + '?limit=' + limit + '&offset=' + offset;
			if (this.pagination.objects.filterByName !== '') {
				url += '&name=' + this.pagination.objects.filterByName;
			}
			this.http({method: 'GET', url: url}).then((res) => {
				this.objects = res.data;
			});
		}

		calculationRequest(object) {
			if (object === 'all') {
				this.http.post('/api/calculation/project/' + this.project.id, {});
			} else {
				this.http.post('/api/calculation/object/' + object.id, {});
			}
		}

		showCalculationResult(type, object) {
			let templateUrl, controller;
			if (type === 'object') {
				templateUrl = 'app/admin/components/admin-project-obj-calc-results/admin-project-obj-calc-results.html';
				controller = 'adminProjectObjCalcResultsController';
			} else {
				//Project
				templateUrl = 'app/admin/components/admin-project-calc-results/admin-project-calc-results.html';
				controller = 'adminProjectCalcResultsController';
			}
			let modalInstance = this.modal.$uibModal.open({
				animation: true,
				keyboard: false,
				backdrop: 'static',
				templateUrl: templateUrl,
				controller: controller,
				size: 'md',
				resolve: {
					project: () => {
						return this.project;
					},
					object: () => {
						return object;
					},
					results: () => {
						let url = object
							? '/api/results/object/' + object.id
							: '/api/results/project/' + this.project.id;

						return this.http({
							method: 'GET',
							url: url
						});
					},
					onDownloadReport: () => {
						//TODO: download calculations report
					}
				}
			});
		}
	}

	angular.module('gigaApp.admin')
		.component('adminProjectSetup', {
			templateUrl: 'app/admin/admin-project-setup/admin-project-setup.html',
			controller: AdminProjectSetupComponent
		});

})();
