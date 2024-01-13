// script to publish to github pages

const ghpages = require('gh-pages');

ghpages.publish(
    'dist',
    () => {
        console.log('Deploy Complete!')
    }
)