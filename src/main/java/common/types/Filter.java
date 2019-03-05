package common.types;

import java.util.List;
import java.util.Set;

/**
 * Base filter.
 */
public interface Filter<T> {

  /**
   * Indicates if the given item meets the criteria of this filter.
   * 
   * @param item is the item to test
   * @return true if the item meets the criteria of this filter, false otherwise
   */
  public boolean meetsCriteria(T item);
  
  /**
   * Returns a new list comprised of elements from the given list that meet the
   * filter's criteria.
   * 
   * @param items are the items to filter
   * @return the items that meet this filter's criteria
   */
  public static <T> List<T> apply(Filter<? extends T> filter, List<T> items) {
    throw new RuntimeException("Not implemented");
  }
  
  /**
   * Returns a new set comprised of elements from the given set that meet the
   * filter's criteria.
   * 
   * @param items are the items to filter
   * @return the items that meet this filter's criteria
   */
  public static <T> Set<T> apply(Filter<? extends T> filter, Set<T> items) {
    throw new RuntimeException("Not implemented");
  }
}
