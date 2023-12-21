'use strict';

(function () {

	class objectParamsComponent {
		//start-non-standard
		projectId = null;
		selectedObjects = [];
		model = {
			objects: [],
			params: {}
		};
		objectParams = [];
		buttonSave = 'Сохранить';
		geo = [];
		dev = [];
		inf = [];
		pow = [];
		pop = [];
		acc = [];
		dem = [];
		//end-non-standard

		constructor($scope, $stateParams, $state, Util, $http) {
			this.scope = $scope;
			this.state = $state;
			this.http = $http;
			this.objectParams = $scope.$parent.$resolve.loadObjectParams.data;
			this.selectedObjects = $stateParams.objects;
			Util.prefillParamsModel(this.objectParams, this.model.params);
			if (this.selectedObjects !== 'all') {
				this.selectedObjectsNames = this.selectedObjects.map(function (i) {
					return i.name;
				});
			}
			this.objectParams.forEach((p) => {
				switch (p.group) {
					case 'GeographicalParametersSet': {
						this.geo.push(p);
					}
						break;
					case 'DevelopmentParametersSet': {
						this.dev.push(p);
					}
						break;
					case 'InfrastructureOptionsSet': {
						this.inf.push(p);
					}
						break;
					case 'PowerOptionsSet': {
						this.pow.push(p);
					}
						break;
					case 'PopulationOptionsSet': {
						this.pop.push(p);
					}
						break;
					case 'AccessLevelOptionsSet': {
						this.acc.push(p);
					}
						break;
					case 'DemandOptionsSet': {
						this.dem.push(p);
					}
						break;
				}
			});
			if ($stateParams.all === true) {
				this.model.objects = 'all';
			} else {
				this.model.objects = this.selectedObjects;
			}
		}

		save() {
			//console.log(this.model);
			const object_id = this.scope.$parent.$resolve.$stateParams.objectId;
      let data_to_store = [];
			for (const key of Object.keys(this.model.params)) {
        data_to_store.push({
          id: key,
          value: this.model.params[key]
        })
      }
      this.http({
        url: '/api/variables/object/'+object_id,
        method: 'POST',
        data: { variables: data_to_store }
      }).then(function(resp) {
        console.log(resp);
      }).catch(function(error) {
        console.log(error);
      })
			//TODO: save model
		}
	}

})();
