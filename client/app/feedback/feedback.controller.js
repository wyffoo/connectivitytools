'use strict';
(function () {

	class FeedbackComponent {
		constructor() {
			this.message = 'Hello';
		}
	}

	angular.module('gigaApp')
		.component('feedback', {
			templateUrl: 'app/feedback/feedback.html',
			controller: FeedbackComponent
		});

})();
