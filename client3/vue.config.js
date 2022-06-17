const { defineConfig } = require('@vue/cli-service')

module.exports = defineConfig({
  devServer: {
    proxy: {
      '/request': {
        target: 'http://localhost:8000'
      }
    }
    // proxy: {
    //   '^/': {
    //     target: 'http://localhost:8000',
    //     changeOrigin: true,
    //     logLevel: 'debug',
    //   },
    // },
    // webSocketServer: false
  },

  pages: {
    index: {
      entry: 'src/EditCharacter.ts'
    },

    test: {
      entry: 'src/SelectCharacterPage.ts'
    }
  }
})