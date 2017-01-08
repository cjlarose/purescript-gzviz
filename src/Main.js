"use strict";

exports.asUint8Array = function (nodeBuffer) {
  return nodeBuffer; // Node Buffers satisfy the API of Uint8Array
};
