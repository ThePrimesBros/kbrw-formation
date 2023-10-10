require('!!file-loader?name=[name].[ext]!./index.html')
/* required library for our React app */
var ReactDOM = require('react-dom')
var React = require("react")
var createReactClass = require('create-react-class')

/* required css for our application */
require('./tuto.webflow/css/orders.css');
require('./tuto.webflow/css/details.css');

var Page = createReactClass({
    render() {
        return <JSXZ in="orders" sel=".container">
        </JSXZ>
    }
})

ReactDOM.render(<Page />, document.getElementById('root'));