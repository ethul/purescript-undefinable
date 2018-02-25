'use strict';

exports['undefined'] = undefined;

exports.fold_ = function(undefined_, defined, value) {
  return value === undefined ? undefined_ : defined(value);
};

