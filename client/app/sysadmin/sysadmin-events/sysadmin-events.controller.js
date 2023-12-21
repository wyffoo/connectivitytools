'use strict';

(function () {

	class SysadminEventsComponent {
		//start-non-standard
		events = [];
		eventTypes = [];
		//end-non-standard

		constructor($scope) {
			this.events = $scope.$parent.$resolve.Events.data;
			this.eventTypes = $scope.$parent.$resolve.EventTypes.data;
		}

		loadEvents(type) {
			//TODO: load particular events by type from server
		}

		select(){
			//TODO select value in eventType or other stuff
		}
	}

	angular.module('gigaApp.sysadmin')
		.component('sysadminEvents', {
			templateUrl: 'app/sysadmin/sysadmin-events/sysadmin-events.html',
			controller: SysadminEventsComponent
		});

})();
