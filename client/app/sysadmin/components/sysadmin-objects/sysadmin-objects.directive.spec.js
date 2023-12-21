'use strict';

describe('Directive: sysadminObjects', function () {

  // load the directive's module and view
  beforeEach(module('gigaApp'));
  beforeEach(module('app/sysadmin/components/sysadmin-objects/sysadmin-objects.html'));

  var element, scope;

  beforeEach(inject(function ($rootScope) {
    scope = $rootScope.$new();
  }));

  it('should make hidden element visible', inject(function ($compile) {
    element = angular.element('<sysadmin-objects></sysadmin-objects>');
    element = $compile(element)(scope);
    scope.$apply();
    expect(element.text()).toBe('this is the sysadminObjects directive');
  }));
});
