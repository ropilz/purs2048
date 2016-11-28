'use strict';

exports.clearConsole = function() {
  document.getElementById('out').innerHTML = "";
};

exports.getChar = function(callback) {
  return function() {
    function grabKey(event) {
      document.body.removeEventListener('keydown', grabKey);
      callback(event.key)();
    }
    document.body.addEventListener('keydown', grabKey);
  };
};

exports.log = function(text) {
  return function() {
    document.getElementById('out').append(text+'\n');
  };
};

exports.printf = function(length) {
  return function(int) {
    const str = int.toString();
    return ' '.repeat(length - str.length) + str;
  };
};