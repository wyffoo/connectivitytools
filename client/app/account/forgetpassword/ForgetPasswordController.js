(function(ng) {
    'use strict';

    ng.module('gigaApp').controller('ForgetPasswordController', ['$scope', '$http', function($scope, $http) {
        $scope.user = {
            email: '',
            code: '',
            newPassword: ''
        };
        $scope.loading = false;
        $scope.alerts = [];

        // Updated handleResponse to process response directly from $http.then
        function handleResponse(response) {
            $scope.loading = false;
            let data;

            try {
                // Attempt to manually parse the JSON to fix malformed response
                data = JSON.parse(response.data.replace(/null$/, ''));
            } catch (e) {
                console.error('Failed to parse response data:', e);
                // Fallback to the response data if parsing fails
                data = response.data ? response.data : {};
            }

            if (response.status >= 200 && response.status < 300) {
                $scope.alerts.push({ type: 'success', msg: data.message || 'Operation successful.' });
            } else {
                let errorMsg = data && data.message ? data.message : 'An unexpected error occurred. Please try again.';
                $scope.alerts.push({ type: 'danger', msg: errorMsg });
            }
        }

        $scope.submitSendCodeForm = function() {
            $scope.loading = true;
            $scope.alerts = [];
            $http.post('/api/users/passwordreset', {
                email: $scope.user.email,
                action: 'sendCode'
            }, {
                headers: { 'Content-Type': 'application/json' },
                transformResponse: function(data) {
                    return data.replace(/null$/, '');
                }
            }).then(function(response) {
                handleResponse(response);
            }, function(response) {
                handleResponse(response);
            });
        };

        $scope.submitResetPasswordForm = function() {
            $scope.loading = true;
            $scope.alerts = [];
            $http.post('/api/users/passwordreset', {
                email: $scope.user.email,
                code: $scope.user.code,
                newPassword: $scope.user.newPassword,
                action: 'verifyCode'
            }, {
                headers: { 'Content-Type': 'application/json' },
                transformResponse: function(data) {
                    return data.replace(/null$/, '');
                }
            }).then(function(response) {
                handleResponse(response);
            }, function(response) {
                handleResponse(response);
            });
        };

        $scope.goBack = function() {
            window.history.back();
        };
    }]);
}(window.angular));
