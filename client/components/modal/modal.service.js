'use strict';

angular.module('gigaApp')
	.factory('Modal', function ($rootScope, $uibModal, $translate) {
		/**
		 * Opens a modal
		 * @param  {Object} scope      - an object to be merged with modal's scope
		 * @param  {String} modalClass - (optional) class(es) to be applied to the modal
		 * @return {Object}            - the instance $uibModal.open() returns
		 */
		let translations = {};
		$translate([
			'components.modal.confirm_delete_title',
			'components.modal.confirm_delete_question',
			'components.modal.confirm_delete_del_button',
			'components.modal.confirm_delete_cancel_button',
			'components.modal.info_close',
			'components.modal.confirm_simple_title',
			'components.modal.confirm_simple_question',
			'components.modal.confirm_simple_confirm_button',
			'components.modal.confirm_simple_cancel_button'
		]).then((result)=>{
			translations = result;
		});

		function decodeHTML (html) {
			var txt = document.createElement('textarea');
			txt.innerHTML = html;
			return txt.value;
		}

		function openModal(scope = {}, modalClass = 'modal-default') {
			var modalScope = $rootScope.$new();
			modalScope.html = scope.modal.html;
			modalScope.text = scope.modal.text;
			modalScope.dismissable = scope.modal.dismissable;
			modalScope.buttons = scope.modal.buttons;
			modalScope.title = scope.modal.title;
			return $uibModal.open({
				templateUrl: 'components/modal/modal.html',
				windowClass: modalClass,
				scope: modalScope
			});
		}

		// Public API here
		return {
			$uibModal: $uibModal,
			/* Confirmation modals */
			info: {
				simple (simple = angular.noop) {

					/**
					 * Open a delete confirmation modal
					 * @param  {String} name   - name or info to show on modal
					 * @param  {All}           - any additional args are passed straight to del callback
					 */
					return function () {
						var args = Array.prototype.slice.call(arguments),
							title = args.shift(),
							text = args.shift(),
							simpleModal;
						simpleModal = openModal({
								dismissable: true,
								title: title,
								html: '<p>' + decodeHTML(text) + '</p>',
								buttons: [{
									classes: 'btn-info',
									text: translations['components.modal.info_close'],
									click: function (e) {
										simpleModal.close(e);
									}
								}]
						}, 'modal-info');
					};
				},
			},
			confirm: {
				/**
				 * Create a function to open a delete confirmation modal (ex. ng-click='myModalFn(name, arg1, arg2...)')
				 * @param  {Function} simple - callback, ran when delete is confirmed
				 * @return {Function}     - the function to open the modal (ex. myModalFn)
				 */
				simple(simple = angular.noop) {
					/**
					 * Open a simple confirmation modal
					 * @param  {String} name   - name or info to show on modal
					 * @param  {All}           - any additional args are passed straight to del callback
					 */
					return function () {
						var args = Array.prototype.slice.call(arguments),
							name = args.shift(),
							deleteModal,
							t = translations;

						deleteModal = openModal({
							modal: {
								dismissable: true,
								title: t['components.modal.confirm_simple_title'],
								html: '<p>' + t['components.modal.confirm_simple_question'] + '<strong>' + name + '</strong></p>',
								buttons: [{
									classes: 'btn-success',
									text: t['components.modal.confirm_simple_confirm_button'],
									click: function (e) {
										deleteModal.close(e);
									}
								}, {
									classes: 'btn-default',
									text: t['components.modal.confirm_simple_cancel_button'],
									click: function (e) {
										deleteModal.dismiss(e);
									}
								}]
							}
						}, 'modal-info');

						deleteModal.result.then(function (event) {
							simple.apply(event, args);
						});
					};
				},

				/**
				 * Create a function to open a delete confirmation modal (ex. ng-click='myModalFn(name, arg1, arg2...)')
				 * @param  {Function} del - callback, ran when delete is confirmed
				 * @return {Function}     - the function to open the modal (ex. myModalFn)
				 */
				delete(del = angular.noop) {
					/**
					 * Open a delete confirmation modal
					 * @param  {String} name   - name or info to show on modal
					 * @param  {All}           - any additional args are passed straight to del callback
					 */
					return function () {
						var args = Array.prototype.slice.call(arguments),
							name = args.shift(),
							deleteModal,
							t = translations;

						deleteModal = openModal({
							modal: {
								dismissable: true,
								title: t['components.modal.confirm_delete_title'],
								html: '<p>' + t['components.modal.confirm_delete_question'] + '<strong>' + name + '</strong> ?</p>',
								buttons: [{
									classes: 'btn-danger',
									text: t['components.modal.confirm_delete_del_button'],
									click: function (e) {
										deleteModal.close(e);
									}
								}, {
									classes: 'btn-default',
									text: t['components.modal.confirm_delete_cancel_button'],
									click: function (e) {
										deleteModal.dismiss(e);
									}
								}]
							}
						}, 'modal-danger');

						deleteModal.result.then(function (event) {
							del.apply(event, args);
						});
					};
				}
			}
		};
	});
