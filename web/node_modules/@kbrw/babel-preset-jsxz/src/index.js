module.exports = function(context, opts) {
  return {
    plugins: [
      [require("@kbrw/babel-plugin-transform-jsxz").default,{templatesDir: opts.dir}]
    ]
  }
}
