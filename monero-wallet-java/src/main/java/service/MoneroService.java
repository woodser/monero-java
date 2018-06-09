package service;

import java.net.URI;
import java.util.List;

import api.MoneroIntegratedAddress;
import api.MoneroUri;

public interface MoneroService {
  
  /**
   * Get a list of available languages for wallet seeds.
   * 
   * @return List<String> is a list of available languages
   */
  public List<String> getLanguages();
  
  /**
   * Create a new wallet.
   * 
   * @param filename is the name of the wallet file to create
   * @param password is the wallet password
   * @param language is the wallet language
   */
  public void createWallet(String filename, String password, String language);
  
  /**
   * Open a wallet.
   * 
   * @param filename is the name of the wallet file to open
   * @param password is the wallet password
   */
  public void openWallet(String filename, String password);
  
  /**
   * Convert a MoneroUri to a standard URI.
   * 
   * @param moneroUri is the MoneroUri to convert to a standard URI
   * @return URI is the MoneroUri converted to a standard URI
   */
  public URI toUri(MoneroUri moneroUri);

  /**
   * Convert a standard URI to a Monero URI.
   * 
   * @param uri is the standard URI to convert
   * @return MoneroUri is the URI converted to a Monero URI
   */
  public MoneroUri toMoneroUri(URI uri);
  
  public MoneroIntegratedAddress splitIntegratedAddress(String integratedAddress);
}
