;;;;;;;;
;; プロキシの設定
;; 以下の形式の環境変数が存在した場合にプロキシの設定を行います。
;; PROXY_HOST_PORT: "proxy.com:8080"
;; PROXY_USER_PASSWORD: "username:password"
;; PROXY_NO_PROXY: "^\(localhost\|10.*\)"
;;;;;;;;
(let ((proxy-host-and-port (getenv "PROXY_HOST_PORT"))
      (proxy-user-and-password (getenv "PROXY_USER_PASSWORD"))
      (proxy-no-proxy (getenv "PROXY_NO_PROXY")))
  (when (and (stringp proxy-host-and-port)
             (stringp proxy-user-and-password)
             (stringp proxy-no-proxy))
    (setq url-proxy-services
          `(("no_proxy" . ,proxy-no-proxy)
            ("http" . ,proxy-host-and-port)
            ("https" . ,proxy-host-and-port)))

    (setq url-http-proxy-basic-auth-storage
          (list `(,proxy-host-and-port
                  ,(cons "Input your LDAP UID !"
                         (base64-encode-string proxy-user-and-password)))))))

;; Local Variables:
;; coding: utf-8-dos
;; End:
