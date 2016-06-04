'use strict';

exports['undefined'] = undefined;

exports.undefinable = function(nothing, just, value){
  return value === undefined ? nothing : just(value);
};

exports.notUndefined = function(value){
  return value;
};
