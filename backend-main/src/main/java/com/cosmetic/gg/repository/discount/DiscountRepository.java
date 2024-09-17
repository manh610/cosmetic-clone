package com.cosmetic.gg.repository.discount;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.discount.Discount;

@Repository
public interface DiscountRepository extends JpaRepository<Discount, String> {
	@Query( value = "SELECT * FROM discount t WHERE (t.code=:key or t.id=:key)", nativeQuery = true)
	Discount findByKey(@Param("key") String key);
	
	@Query( value = "SELECT t.id AS id, t.code AS code, t.name AS name, t.start_date AS startDate, " +
			"t.end_date AS endDate, t.level AS level, t.path AS path, t.discount_type AS discountType, " +
			"t.is_show AS isShow, t.image AS image, t.description AS description, " +
			"FROM discount t " +
			"INNER JOIN user_discount t2 ON t2.discount_id=t.id " +
			"INNER JOIN user"
			, nativeQuery = true)
	Object detail(@Param("id") String id);
	
	@Query( value = "SELECT count(*) FROM discount t " +
			"INNER JOIN user_discount t2 ON t.id=t2.discount_id " +
			"WHERE (t.id=:id AND t2.is_use=true) "
			, nativeQuery = true)
	Integer cntUse(@Param("id") String id);
	
	@Query(value = "SELECT * FROM discount a WHERE " +
		    "(CASE WHEN :discountType IS NOT NULL THEN a.discount_type=:discountType ELSE (a.discount_type='VOUCHER' or a.discount_type='PROMOTION') END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.code REGEXP :keyword " +
		    "OR a.name REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) " +
		    "ORDER BY a.start_date DESC, a.end_date DESC, a.name ASC LIMIT :pageSize OFFSET :pageIndex",
		    nativeQuery = true)
	List<Discount> search(
			@Param("keyword") String keyword,
			@Param("discountType") String discountType,
			@Param("pageIndex") Integer pageIndex,
            @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT count(*) FROM discount a WHERE " +
		    "(CASE WHEN :discountType IS NOT NULL THEN a.discount_type=:discountType ELSE (a.discount_type='VOUCHER' or a.discount_type='PROMOTION') END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.code REGEXP :keyword " +
		    "OR a.name REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) ",
		    nativeQuery = true)
	Integer cntDiscount(
			@Param("keyword") String keyword,
			@Param("discountType") String discountType);
	
	
	
	
	@Query( value = "SELECT * FROM discount t INNER JOIN user_discount t2 ON t2.discount_id=t.id " +
			"WHERE (t2.user_id=:id AND t2.is_use=false AND t.end_date>=NOW() AND "+
			"(CASE WHEN :use IS NOT NULL THEN t2.is_use=:use ELSE (1=1) END))"
			, nativeQuery = true)
	List<Discount> getDiscountByUser(@Param("id") String id, @Param("use") Boolean use);
	
	@Query( value = "SELECT * FROM discount t INNER JOIN user_discount t2 ON t2.discount_id=t.id " +
			"WHERE (t2.user_id=:userId AND t2.is_use=false AND t.end_date>=NOW() AND t.id=:discountId)"
			, nativeQuery = true)
	Discount checkDiscountByUser(@Param("discountId") String discountId,
			@Param("userId") String userId);
	
	@Query(value = "SELECT t.id AS id, t.code AS code, t.name AS name, t.start_date AS startDate, " +
			"t.end_date AS endDate, t.value AS value, t.path AS path, t.discount_type AS discountType, " +
			"t.is_show AS isShow, t.image AS image, t.description AS description, t2.quantity AS quantity " +
			"FROM discount t INNER JOIN product_item_discount t2 ON t2.discount_id=t.id " +
			"WHERE (t2.product_item_id=:productItemId AND t.end_date>=NOW() AND t2.quantity>0) "
			, nativeQuery = true)
	List<Object> getDiscountByProductItem(@Param("productItemId") String productItemId);
	
	@Query(value = "SELECT t.id AS id, t.code AS code, t.name AS name, t.start_date AS startDate, " +
			"t.end_date AS endDate, t.value AS value, t.path AS path, t.discount_type AS discountType, " +
			"t.is_show AS isShow, t.image AS image, t.description AS description, t2.quantity AS quantity " +
			"FROM discount t INNER JOIN product_discount t2 ON t2.discount_id=t.id " +
			"WHERE (t2.product_id=:productId AND t.end_date>=NOW() AND t2.quantity>0) "
			, nativeQuery = true)
	List<Object> getDiscountByProduct(@Param("productId") String productId);
}
