defmodule Match do
  @email ~r/^([a-z0-9]{1}[a-z0-9_.+-]{1,62}[a-z0-9]{1})@([a-z0-9]{1}[a-z0-9.-]{1,100}[a-z0-9]{1})$/
  @url ~r/^(https?):\/\/[a-z0-9]{0,1}[a-z0-9-]{0,61}[a-z0-9]{1,1}\.[a-z]{1,}[-a-zA-Z0-9+&@#\/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#\/%=~_|]$/
  @phone ~r/^\+{1}[0-9]{11,15}$/
  @hostname ~r/^([a-zA-Z0-9][a-zA-Z0-9\-]{0,61}\.)*[a-zA-Z0-9][a-zA-Z0-9\-]{0,61}[a-zA-Z0-9]\.[a-zA-Z0-9]{2,}$/
  # @wallet_address ~r/(1x)[1-9A-HJ-NP-Za-km-z]+$/
  @wallet_address ~r/(0x|1x)[1-9A-HJ-NP-Za-km-z]+$/
  @domain ~r/^[A-Za-z0-9][A-Za-z0-9-]{1,61}[A-Za-z0-9]\.[A-Za-z]{1,}$/
  @subdomain ~r/^([a-z0-9]{1}[a-z0-9-]?){0,62}[a-z0-9]{1}.$/
  @ippan_domain ~r/^[a-z0-9]{0,1}[a-z0-9-]{0,61}[a-z0-9]{1,1}\.(cmm|npo|ntw|cyber|ipn|wlt|iwl|ippan|btc|cyb|fin|geo|and|gold|god|lux|yes|bbb|i|u|btw|nws|diy|iot|69|opasy|ops|avatar|ultra|more|daddy|bro|sister|fck|tribe|mogul|tequila|gpt|soho|voice|eye|hodl|linux|youxi|we|genius|ciao|ok|dns|cyborg|replicant|air|amigo|bbq|burger|diamond|invest|jewel|pop|rap|rice|rod|soft|tkt|toy|vida|zoom|papi|hola)$/
  @hashtag ~r/(?:$|)#[A-Za-z0-9\-\.\_]+(?:$|)/
  @base58 ~r/^[1-9A-HJ-NP-Za-km-z]+$/
  @base62 ~r/^[0-9A-Za-z]+$/
  @domain_link ~r/^@([a-z0-9]{0,1}[a-z0-9-]{0,61}[a-z0-9]{1,1})/
  # @username ~r/((?!^[\.\-\_])([a-z0-9\.\-\_])(?![\.\_\-][\.\_\-])(?![\.\-\_]$)){1,30}/
  @username ~r/^[a-z0-9]+([._-]?[a-z0-9]+)*$/
  @token ~r/^[A-Z0-9]{1,10}$/
  @no_binary ~r/^[A-Za-z0-9:punct:\s]+$/

  def hostname?(x), do: Regex.match?(@hostname, x)
  def domain?(x), do: Regex.match?(@domain, x)
  def subdomain?(x), do: Regex.match?(@subdomain, x)
  def ippan_domain?(x), do: Regex.match?(@ippan_domain, x)
  def url?(x), do: Regex.match?(@url, x)
  def email?(x), do: Regex.match?(@email, x)
  def hashtag(x), do: Regex.match?(@hashtag, x)
  def base58(x), do: Regex.match?(@base58, x)
  def base62(x), do: Regex.match?(@base62, x)
  def domain_link(x), do: Regex.match?(@domain_link, x)
  def phone?(x), do: Regex.match?(@phone, x)
  def wallet_address?(x), do: Regex.match?(@wallet_address, x)
  def username?(x), do: Regex.match?(@username, x)
  def token?(x), do: Regex.match?(@token, x)
  def no_binary(x), do: Regex.match?(@no_binary, x)

  def account?(x) do
    Regex.match?(@wallet_address, x)
    # byte_size(x) <= 20 and Regex.match?(@username, x)
  end
end
