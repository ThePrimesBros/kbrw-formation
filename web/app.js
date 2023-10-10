require('!!file-loader?name=[name].[ext]!./index.html')
/* required library for our React app */
let ReactDOM = require('react-dom')
let React = require("react")
let createReactClass = require('create-react-class')

/* required css for our application */
require('./tuto.webflow/css/orders.css');
require('./tuto.webflow/css/details.css');

var Page = createReactClass({
    render() {
        var orders = [
            { remoteid: "000000189", custom: { customer: { full_name: "TOTO & CIE" }, billing_address: "Some where in the world" }, items: 2 },
            { remoteid: "000000190", custom: { customer: { full_name: "Looney Toons" }, billing_address: "The Warner Bros Company" }, items: 3 },
            { remoteid: "000000191", custom: { customer: { full_name: "Asterix & Obelix" }, billing_address: "Armorique" }, items: 29 },
            { remoteid: "000000192", custom: { customer: { full_name: "Lucky Luke" }, billing_address: "A Cowboy doesn't have an address. Sorry" }, items: 0 },
        ]
        return <JSXZ in="orders" sel=".container">
            {
                orders.map(order => (<JSXZ in="orders" sel=".list-2" key={order.remoteid}>
                    <Z sel=".id-2">{order.remoteid}</Z>
                    <Z sel=".name-2">{order.custom.customer.full_name}</Z>
                    <Z sel=".adresse-2">{order.custom.billing_address}</Z>
                    <Z sel=".items-2">{order.items}</Z>
                </JSXZ>))
            }
        </JSXZ>
    }
})

ReactDOM.render(
    <Page />,
    document.getElementById('root')
);