// https://github.com/clintonwoo/hackernews-react-graphql/blob/master/src/components/presentational/HeaderNav.js
    <span className="pagetop">
      <b className="hnname">
        <Link prefetch href="/" as="/news">
          <a>
            {props.title}
          </a>
        </Link>
      </b>
      &nbsp;
      {props.userId &&
      <Link prefetch href="/newswelcome">
        <a>
          welcome
        </a>
      </Link>
      }
      {props.userId && ' | '}
      <Link prefetch href="/newest">
        <a className={props.currentURL === '/newest' ? 'topsel' : ''}>
          new
        </a>
      </Link>
      {props.userId && ' | '}
      {props.userId &&
      <Link prefetch href={`/threads?id=${props.userId}`}>
        <a className={props.currentURL === '/threads' ? 'topsel' : ''}>
          threads
        </a>
      </Link>
      }
      {' | '}
      <Link prefetch href="/newcomments">
        <a className={props.currentURL === '/newcomments' ? 'topsel' : ''}>
          comments
        </a>
      </Link>
      {' | '}
      <Link prefetch href="/show">
        <a className={props.currentURL === '/show' ? 'topsel' : ''}>
          show
        </a>
      </Link>
      {' | '}
      <Link prefetch href="/ask">
        <a className={props.currentURL === '/ask' ? 'topsel' : ''}>
          ask
        </a>
      </Link>
      {' | '}
      <Link prefetch href="/jobs">
        <a className={props.currentURL === '/jobs' ? 'topsel' : ''}>
          jobs
        </a>
      </Link>
      {' | '}
      <Link prefetch href="/submit">
        <a className={props.currentURL === '/submit' ? 'topsel' : ''}>
          submit
        </a>
      </Link>
      {props.currentURL === '/best' && ' | '}
      {props.currentURL === '/best' && (
      <Link prefetch href="/best">
        <a className="topsel">
          best
        </a>
      </Link>
      )
      }
    </span>
