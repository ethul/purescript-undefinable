'use strict';

exports['undefined'] = undefined;

exports.undefinable = function undefinable(nothing, just, value){
  return value === undefined ? nothing : just(value);
};

exports.notUndefined = function notUndefined(value){
  return value;
};
