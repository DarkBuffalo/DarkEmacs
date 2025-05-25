



(package! mastodon
  :bind
  (:map dark-open-map
        ("m" . mastodon))
  :config
  (mastodon-discover))
