defmodule Match do
  @email ~r/^([a-z0-9]{1}[a-z0-9_.+-]{1,62}[a-z0-9]{1})@([a-z0-9]{1}[a-z0-9.-]{1,100}[a-z0-9]{1})$/
  @url ~r/^(https?):\/\/[a-z0-9]{0,1}[a-z0-9-]{0,61}[a-z0-9]{1,1}\.[a-z]{1,}[-a-zA-Z0-9+&@#\/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#\/%=~_|]$/
  @phone ~r/^\+{1}[0-9]{11,15}$/
  @hostname ~r/^([a-zA-Z0-9][a-zA-Z0-9\-]{0,61}\.)*[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]\.[a-zA-Z0-9]{2,}$/
  @ipv4 ~r/^((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])$/
  @host_ipv4 ~r/^(([a-zA-Z0-9][a-zA-Z0-9\-]{0,61}\.)*[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]\.[a-zA-Z0-9]{2,})|(((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))$/
  @wallet_address ~r/^(\d+)x[1-9A-HJ-NP-Za-km-z]+$/
  @domain ~r/^[A-Za-z0-9][A-Za-z0-9-]{1,61}[A-Za-z0-9]\.[A-Za-z]{1,}$/
  @subdomain ~r/^([a-z0-9]{1}[a-z0-9-]?){0,62}[a-z0-9]{1}.$/
  @ippan_domain ~r/^[a-z0-9]{0,1}[a-z0-9-]{0,61}[a-z0-9]{1,1}\.ipn$/
  @hashtag ~r/(?:$|)#[A-Za-z0-9\-\.\_]+(?:$|)/
  @base16 ~r/^[0-9A-Fa-f]+$/
  @base58 ~r/^[1-9A-HJ-NP-Za-km-z]+$/
  @base62 ~r/^[0-9A-Za-z]+$/
  @domain_link ~r/^@([a-z0-9]{0,1}[a-z0-9-]{0,61}[a-z0-9]{1,1})/
  @username ~r/^@[a-z0-9]{1}([a-z0-9_]{2,20})$/
  @account ~r/^((\d+)x[1-9A-HJ-NP-Za-km-z]+$)|@[a-z0-9]{1}([a-z0-9_]{2,24})$/
  @service ~r/^s-[a-z0-9]{1}([a-z0-9_]{2,20})$/
  @token ~r/^[A-Z0-9]{1,10}$/
  @text ~r/^[\x20-\x26|\x28-\x7E]+$/

  def hostname?(x), do: Regex.match?(@hostname, x)
  def domain?(x), do: Regex.match?(@domain, x)
  def subdomain?(x), do: Regex.match?(@subdomain, x)
  def ippan_domain?(x), do: Regex.match?(@ippan_domain, x)
  def url?(x), do: Regex.match?(@url, x)
  def email?(x), do: Regex.match?(@email, x)
  def hashtag(x), do: Regex.match?(@hashtag, x)
  def base16(x), do: Regex.match?(@base16, x)
  def base58(x), do: Regex.match?(@base58, x)
  def base62(x), do: Regex.match?(@base62, x)
  def domain_link(x), do: Regex.match?(@domain_link, x)
  def phone?(x), do: Regex.match?(@phone, x)
  def wallet_address?(x), do: Regex.match?(@wallet_address, x)
  def username?(x), do: Regex.match?(@username, x)
  def service?(x), do: Regex.match?(@service, x)
  def token?(x), do: Regex.match?(@token, x)
  def text?(x), do: Regex.match?(@text, x)
  def ipv4?(x), do: Regex.match?(@ipv4, x)
  def host_or_ipv4?(x), do: Regex.match?(@host_ipv4, x)

  def account?(x) do
    Regex.match?(@account, x)
  end
end
