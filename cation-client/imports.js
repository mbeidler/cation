// CSS
require('./app.css')

/**
 * react-flux dependencies
 */
var React = require('react');
var ReactDOM = require('react-dom');

// Bootstrap
window.jQuery = require ('jquery');
window.Tether = require('tether');
var bootstrap = require('bootstrap');

// ReactableJS for grid
var Reactable = require('reactable');

// Attach to window
window.React = React;
window.ReactDOM = ReactDOM;
window.Table = Reactable.Table;
window.Thead = Reactable.Thead;
window.Th = Reactable.Th;
window.Tr = Reactable.Tr;
window.Td = Reactable.Td;

