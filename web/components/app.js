require('!!file-loader?name=[name].[ext]!./../layout.html.eex')

let ReactDOM = require('react-dom');
let React = require("react")
let createReactClass = require('create-react-class')
let Qs = require('qs')
let Cookie = require('cookie')
let localhost = require('reaxt/config').localhost
let XMLHttpRequest = require("xhr2") // External XmlHTTPReq on browser, xhr2 on server

require('../tuto.webflow/css/orders.css');
require('../tuto.webflow/css/details.css');

let routes = {
  "orders": {
    path: (params) => {
      return "/";
    },
    match: (path) => {
      return (path == "/") && { handlerPath: [Layout, Header, Orders] } // Note that we use the "&&" expression to simulate a IF statement
    }
  },
  "order": {
    path: (params) => {
      return "/order/" + params;
    },
    match: (path, qs) => {
      let r = new RegExp("/order/([^/]*)$").exec(path)
      return r && { handlerPath: [Layout, Header, Order], order_id: r[1] } // Note that we use the "&&" expression to simulate a IF statement
    }
  }
}

let HTTP = new (function () {
  this.get = (url) => this.req('GET', url)
  this.delete = (url) => this.req('DELETE', url)
  this.post = (url, data) => this.req('POST', url, data)
  this.put = (url, data) => this.req('PUT', url, data)

  this.req = (method, url, data) => {
    return new Promise((resolve, reject) => {
      let req = new XMLHttpRequest()
      url = (typeof window !== 'undefined') ? url : localhost + url
      req.open(method, url)
      req.responseType = "text"
      req.setRequestHeader("accept", "application/json,*/*;0.8")
      req.setRequestHeader("content-type", "application/json")
      req.onload = () => {
        if (req.status >= 200 && req.status < 300) {
          resolve(req.responseText && req.responseText)
        } else {
          reject({ http_code: req.status })
        }
      }
      req.onerror = (err) => {
        reject({ http_code: req.status })
      }
      req.send(data && JSON.stringify(data))
    })
  }
})()

let remoteProps = {
  user: (props) => {
    return {
      url: "/api/me",
      prop: "user"
    }
  },
  orders: (props) => {
    // if (!props.user) return
    // , user_id: props.user.value.id
    let qs = { ...props.qs }
    let query = Qs.stringify(props.qs)
    return {
      url: "/api/orders" + (query == '' ? '' : '?' + query),
      prop: "orders"
    }
  },
  order: (props) => {
    return {
      url: "/api/order/" + props.order_id,
      prop: "order"
    }
  }
}

let Child = createReactClass({
  render() {
    let [ChildHandler, ...rest] = this.props.handlerPath
    return <ChildHandler {...this.props} handlerPath={rest} />
  }
})

class Layout extends React.Component {
  state = {
    modal: null,
    loading: false,
    searchQuery: "",
    page: 0,
  };

  loader = (promise) => {
    this.setState({ loading: true }); // Show the loader
    return new Promise((resolve, reject) => {
      promise
        .then((result) => {
          this.setState({ loading: false }); // Hide the loader
          resolve(result);
        })
        .catch((error) => {
          this.setState({ loading: false }); // Hide the loader
          reject(error);
        });
    });
  };

  modal = (spec) => {
    this.setState({
      modal: {
        ...spec, callback: (res) => {
          this.setState({ modal: null }, () => {
            if (spec.callback) spec.callback(res);
          });
        }
      }
    });
  };

  handlePageChange = (delta) => {
    const newPage = this.state.page + delta;
    if (newPage < 0) {
      return; // Prevent going below 0
    }

    this.setState({ page: newPage }, () => {
      // Update the URL after the state has changed
      const url = new URL(window.location.href);
      url.href = url.href.replace("#", "")

      url.searchParams.set('page', newPage);
      window.history.replaceState(null, '', url.toString());
    });
  }

  handleQueryChange = () => {
    let searchQuery = document.getElementsByClassName(".url-changer");
    this.setState({ searchQuery: searchQuery }), () => {
      const url = new URL(window.location.href);
      url.href = url.href.replace("#", "")

      url.searchParams.set('q', newPage);
      window.history.replaceState(null, '', url.toString());
    }
  }

  render() {
    let props = {
      ...this.props, modal: this.modal,
    }
    const { loading } = this.state;
    let modal_component;

    if (this.state.modal && this.state.modal.type) {
      modal_component = {
        'delete': (props) => <DeleteModal {...props} modalData={this.state.modal} />
      };
    }

    modal_component = modal_component && modal_component[this.state.modal.type](this.state.modal);

    return (
      <JSXZ in="orders" sel=".layout">
        <Z sel=".modal-wrapper" className={cn(classNameZ, { 'hidden': !modal_component })}>
          {modal_component}
        </Z>
        <Z sel=".container">
          {loading ? <Loader /> : null} {/* Render the loader when loading is true */}
          <this.props.Child {...props} />
        </Z>
        <Z sel=".url-changer"><ChildrenZ /></Z>
        <Z sel=".button-url" onClick={this.handleQueryChange}><ChildrenZ /></Z>
        <Z sel=".container">
          <this.props.Child {...props} />
        </Z>
        <Z sel=".btn-previous" onClick={() => this.handlePageChange(-1)}><ChildrenZ /></Z>
        <Z sel=".btn-next" onClick={() => this.handlePageChange(1)}><ChildrenZ /></Z>
      </JSXZ>
    );
  }
};

let DeleteModal = createReactClass({
  render() {
    const handleAccept = () => {
      this.props.callback(this.props.orderId)
    }

    const handleDecline = () => {
      this.props.callback();
    }
    return <JSXZ in="orders" sel=".modal-container">
      <Z sel=".modal-title">{this.props.title}</Z>
      <Z sel=".modal-para">{this.props.message}</Z>
      <Z sel=".button-accept" onClick={handleAccept}><ChildrenZ /></Z>
      <Z sel=".button-decline" onClick={handleDecline}><ChildrenZ /></Z>
    </JSXZ>
  }
})

let Loader = createReactClass({
  render() {
    return <JSXZ in="orders" sel=".loader-wrapper">
      <Z sel=".loader-container"><ChildrenZ /></Z>
    </JSXZ>
  }
})

let ErrorPage = createReactClass({
  render() {
    return <div>{this.props.message}</div>
  }
})

let Header = createReactClass({
  render() {
    return <JSXZ in="orders" sel=".headers">
      <Z sel=".headers-container">
        <this.props.Child {...this.props} />
      </Z>
    </JSXZ>
  }
})

let Orders = createReactClass({
  statics: {
    remoteProps: [remoteProps.orders]
  },

  render() {
    const handleClick = (order_id) => {
      this.props.modal({
        type: 'delete',
        title: 'Order deletion',
        message: `Are you sure you want to delete this ?`,
        orderId: order_id,
        callback: (value = "") => {
          if (value != "") {
            fetch(`http://localhost:8080/api/delete/${value}`, {
              method: "DELETE"
            }).then(function (res) {
              window.location.assign("http://localhost:8080")
            })
          } else {
            return Promise.resolve();
          }
        }
      })
    }

    return <JSXZ in="orders" sel=".orders">
      {
        JSON.parse(this.props.orders.value).results.map(order => (<JSXZ in="orders" sel=".list-2" key={order.id}>
          <Z sel=".id-2">{order.remoteid}</Z>
          <Z sel=".name-2">{order.custom.customer.full_name}</Z>
          <Z sel=".adresse-2">{order.custom.billing_address.street}</Z>
          <Z sel=".items-2">{order.custom.items.length}</Z>
          <Z sel=".w-button" onClick={(e) => {
            e.preventDefault()
            this.props.Link.GoTo("order", order.remoteid, {})
          }}><ChildrenZ />
          </Z>
          <Z sel=".delete" onClick={(e) = () => handleClick(order.remoteid)}>
            <ChildrenZ />
          </Z>
        </JSXZ>))
      }
    </JSXZ>
  }
})

let Order = createReactClass({
  statics: {
    remoteProps: [remoteProps.order]
  },
  render() {
    return <JSXZ in="detail" sel=".container-role">
      {/* <Z sel=".order-client">{JSON.parse(this.props.order.value).data.custom.customer.full_name}</Z>
      <Z sel=".order-adress">{JSON.parse(this.props.order.value).data.custom.billing_address.street}</Z>
      <Z sel=".order-title">{`Commande n° ${JSON.parse(this.props.order.value).id}`}</Z> */}
      {
        JSON.parse(this.props.order.value).data.custom.items.map(item => (
          <JSXZ in="detail" sel=".item-list" key={item.item_id}>
            <Z sel=".product-name">{item.product_title}</Z>
            <Z sel=".quantity">{item.quantity_to_fetch}</Z>
            <Z sel=".unit-price">{item.unit_price}</Z>
            <Z sel=".total-price">{item.price * item.quantity_to_fetch}</Z>
          </JSXZ>))
      }
    </JSXZ>
  }
})

let Link = createReactClass({
  statics: {
    renderFunc: null, //render function to use (differently set depending if we are server sided or client sided)
    GoTo(route, params, query) {// function used to change the path of our browser
      let qs = Qs.stringify(query)
      let url = routes[route].path(params) + ((qs == '') ? '' : ('?' + qs))
      history.pushState({}, "", url)
      Link.onPathChange()
    },
    onPathChange() { //Updated onPathChange
      let path = location.pathname
      let qs = Qs.parse(location.search.slice(1))
      let cookies = Cookie.parse(document.cookie)
      inferPropsChange(path, qs, cookies).then( //inferPropsChange download the new props if the url query changed as done previously
        () => {
          Link.renderFunc(<Child {...browserState} />) //if we are on server side we render
        }, ({ http_code }) => {
          Link.renderFunc(<ErrorPage message={"Not Found"} code={http_code} />, http_code) //idem
        }
      )
    },
    LinkTo: (route, params, query) => {
      let qs = Qs.stringify(query)
      return routes[route].path(params) + ((qs == '') ? '' : ('?' + qs))
    }
  },
  onClick(ev) {
    ev.preventDefault();
    Link.GoTo(this.props.to, this.props.params, this.props.query);
  },
  render() {//render a <Link> this way transform link into href path which allows on browser without javascript to work perfectly on the website
    return (
      <a href={Link.LinkTo(this.props.to, this.props.params, this.props.query)} onClick={this.onClick}>
        {this.props.children}
      </a>
    )
  }
})

const cn = function () {
  let args = arguments, classes = {}
  for (let i in args) {
    let arg = args[i]
    if (!arg) continue
    if ('string' === typeof arg || 'number' === typeof arg) {
      arg.split(" ").filter((c) => c != "").map((c) => {
        classes[c] = true
      })
    } else if ('object' === typeof arg) {
      for (let key in arg) classes[key] = arg[key]
    }
  }
  return Object.keys(classes).map((k) => classes[k] && k || '').join(' ')
}

function addRemoteProps(props) {
  return new Promise((resolve, reject) => {
    let remoteProps = Array.prototype.concat.apply([],
      props.handlerPath
        .map((c) => c.remoteProps) // -> [[remoteProps.orders], null]
        .filter((p) => p) // -> [[remoteProps.orders]]
    )

    remoteProps = remoteProps
      .map((spec_fun) => spec_fun(props)) // [{url: '/api/orders', prop: 'orders'}]
      .filter((specs) => specs) // get rid of undefined from remoteProps that don't match their dependencies
      .filter((specs) => !props[specs.prop] || props[specs.prop].url != specs.url) // get rid of remoteProps already resolved with the url
    if (remoteProps.length == 0)
      return resolve(props)


    // All remoteProps can be queried in parallel. This is just the function definition, see its use below.
    const promise_mapper = (spec) => {
      // we want to keep the url in the value resolved by the promise here : spec = {url: '/api/orders', value: ORDERS, prop: 'orders'}
      return HTTP.get(spec.url).then((res) => { spec.value = res; return spec })
    }

    const reducer = (acc, spec) => {
      // spec = url: '/api/orders', value: ORDERS, prop: 'user'}
      acc[spec.prop] = { url: spec.url, value: spec.value }
      return acc
    }

    const promise_array = remoteProps.map(promise_mapper)
    return Promise.all(promise_array)
      .then(xs => xs.reduce(reducer, props), reject)
      .then((p) => {
        // recursively call remote props, because props computed from
        // previous queries can give the missing data/props necessary
        // to define another query
        return addRemoteProps(p).then(resolve, reject)
      }, reject)
  })
}

// let GoTo = (route, params, query) => {
//   let qs = Qs.stringify(query)
//   let url = routes[route].path(params) + ((qs=='') ? '' : ('?'+qs))
//   history.pushState({}, "", url)
//   return onPathChange()
// }

// let oldBrowserState = { Child: Child, GoTo: GoTo }

// function onPathChange() {
//   let path = location.pathname
//   let qs = Qs.parse(location.search.slice(1))
//   let cookies = Cookie.parse(document.cookie)

//   oldBrowserState = {
//     ...oldBrowserState,
//     path: path,
//     qs: qs,
//     cookie: cookies
//   }
//   let route

//   for (let key in routes) {
//     routeProps = routes[key].match(path, qs)
//     if (routeProps) {
//       route = key
//       break;
//     }
//   }

//   if (!route) return ReactDOM.render(<ErrorPage message={"Not Found"} code={404} />, document.getElementById('root'))

//   oldBrowserState = {
//     ...oldBrowserState,
//     ...routeProps,
//     route: route
//   }
//   // console.log("browserState in onPathChange", browserState)
//   addRemoteProps(oldBrowserState).then(
//     (props) => {
//       browserState = props

//       ReactDOM.render(<Child {...browserState} />, document.getElementById('root'))
//     }, (res) => {
//       ReactDOM.render(<ErrorPage message={"Shit happened"} code={res.http_code} />, document.getElementById('root'))
//     })
// }

let browserState = {}

function inferPropsChange(path, query, cookies) { // the second part of the onPathChange function have been moved here
  browserState = {
    ...browserState,
    path: path, qs: query,
    Link: Link,
    Child: Child
  }

  let route, routeProps
  for (let key in routes) {
    routeProps = routes[key].match(path, query)
    if (routeProps) {
      route = key
      break
    }
  }

  if (!route) {
    return new Promise((res, reject) => reject({ http_code: 404 }))
  }
  browserState = {
    ...browserState,
    ...routeProps,
    route: route
  }

  return addRemoteProps(browserState).then(
    (props) => {
      browserState = props
    })
}

module.exports = {
  reaxt_server_render(params, render) {
    browserState = {}
    inferPropsChange(params.path, params.query, params.cookies)
      .then(() => {
        render(<Child {...browserState} />)
      }, (err) => {
        render(<ErrorPage message={"Not Found"} code={err.http_code} />, err.http_code)
      })
  },
  reaxt_client_render(initialProps, render) {
    // Retrieve initial props to render from Reaxt (the one use on server-side) then render
    browserState = {
      ...initialProps
    }

    // clean it to remove badly transmitted props
    Object.keys(initialProps).map((k) => {
      let val = initialProps[k]
      if (val && val.value) browserState[k] = val
    })

    Link.renderFunc = render

    window.addEventListener("popstate", () => {
      Link.onPathChange()
    }) // inferProps and render on history change
    Link.onPathChange()
  }
}