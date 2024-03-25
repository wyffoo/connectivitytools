(function(ng) {
    'use strict';

    ng.module('gigaApp').controller('ForgetPasswordController', ['$scope', '$http', function($scope, $http) {
        $scope.user = {
            email: '',
            code: '',
            newPassword: ''
        };
        $scope.loading = false;
        $scope.alertMessage = '';
        $scope.alertType = '';

        // Function to send the verification code
        $scope.submitSendCodeForm = function() {
            $scope.loading = true;
            $http.post('/api/users/passwordreset', {
                email: $scope.user.email,
                action: 'sendCode'
            }, {
                headers: { 'Content-Type': 'application/json' }
            }).then(function(response) {
                // Handle success directly
                handleResponse(response, false);
            }).catch(function(error) {
                // Handle potential success disguised as error
                handleResponse(error, true);
            });
        };

        // Function to verify the code and reset the password
        $scope.submitResetPasswordForm = function() {
            $scope.loading = true;
            $http.post('/api/users/passwordreset', {
                email: $scope.user.email,
                code: $scope.user.code,
                newPassword: $scope.user.newPassword,
                action: 'verifyCode'
            }, {
                headers: { 'Content-Type': 'application/json' }
            }).then(function(response) {
                // Handle success directly
                handleResponse(response, false);
            }).catch(function(error) {
                // Handle potential success disguised as error
                handleResponse(error, true);
            });
        };

        // Function to handle responses, both successful and errors
        function handleResponse(response, isError) {
            var data = isError ? response.data : response.data;
            var status = isError ? response.status : response.status;

            if (status === 200) {
                $scope.alertMessage = data.message;
                $scope.alertType = 'success';
            } else {
                $scope.alertMessage = data && data.message ? data.message : 'An unexpected error occurred. Please try again.';
                $scope.alertType = 'danger';
            }
            $scope.loading = false;
        }

        // Function to go back to the previous page
        $scope.goBack = function() {
            window.history.back();
        };
    }]);
}(window.angular));
