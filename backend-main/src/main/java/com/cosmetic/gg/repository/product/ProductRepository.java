package com.cosmetic.gg.repository.product;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import com.cosmetic.gg.entity.product.Product;

@Repository
public interface ProductRepository extends JpaRepository<Product, String>{

	@Query( value = "SELECT * FROM product t WHERE (t.code=:key or t.id=:key)", nativeQuery = true)
	Product findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM product t WHERE (t.brand_id=:id or t.category_id=:id)", nativeQuery = true)
	Product findByAttribute(@Param("id") String id);
	
	@Query(value = "SELECT * FROM product t INNER JOIN product_skin_type t2 ON t2.product_id=t.id " +
			"WHERE t2.skin_type_id=:id", nativeQuery = true)
	Product findBySkinType(@Param("id") String id);
	
	@Query( value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, a.expiration_date AS expirationDate, " +
			"a.category_id AS categoryId, a.brand_id AS brandId, c.name AS categoryName, b.name AS brandName, " +
			"min(t3.sell_price) AS minPrice, max(t3.sell_price) AS maxPrice, sum(DISTINCT t3.sell_quantity) AS totalQuantity, " +
			"GROUP_CONCAT(DISTINCT CONCAT(t5.id, ':', t5.name) ORDER BY t5.name) AS skinTypes " +
			"FROM product a INNER JOIN category c ON c.id=a.category_id " +
			"INNER JOIN brand b ON b.id=a.brand_id " +
			"INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"INNER JOIN skin_type t5 ON t5.id=t4.skin_type_id " + 
			"WHERE a.id=:id "
			, nativeQuery = true)
	Object detail(@Param("id") String id);
	
	@Query(value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, " +
			"a.expiration_date AS expirationDate, a.category_id AS categoryId, a.brand_id AS brandId, " +
			"c.name AS categoryName, b.name AS brandName, min(t3.sell_price) AS minPrice, max(t3.sell_price) AS maxPrice, " +
			"GROUP_CONCAT(DISTINCT CONCAT(t5.id, ':', t5.name) ORDER BY t5.name) AS skinTypes " +
			"FROM product a INNER JOIN category c ON c.id=a.category_id " +
			"INNER JOIN brand b ON b.id=a.brand_id " +
			"INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"INNER JOIN skin_type t5 ON t5.id=t4.skin_type_id " +
			"WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (a.status <> '') END) AND " +
		    "(CASE WHEN :brandId IS NOT NULL THEN a.brand_id=:brandId ELSE (1=1) END) AND " +
		    "(CASE WHEN :categoryId IS NOT NULL THEN a.category_id=:categoryId ELSE (1=1) END) AND " +
		    "(CASE WHEN :skinTypeId IS NOT NULL THEN t4.skin_type_id=:skinTypeId ELSE (1=1) END) AND " +
		    "(CASE WHEN (:min IS NOT NULL AND :max IS NOT NULL) THEN t3.sell_price BETWEEN :min and :max ELSE (1=1) END) AND " +
		    "(CASE WHEN :isDate IS NOT NULL THEN " +
		    	"(CASE WHEN :isDate=true THEN a.expiration_date < NOW() ELSE a.expiration_date >= NOW() END) " +
		    " ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.name REGEXP :keyword " +
		    "OR a.code REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) " +
		    "GROUP BY a.id LIMIT :pageSize OFFSET :pageIndex",
		    nativeQuery = true)
	List<Object> searchBySkinType(@Param("keyword") String keyword,
					@Param("status") String status,
                      @Param("brandId") String brandId,
                      @Param("categoryId") String categoryId,
                      @Param("skinTypeId") String skinTypeId,
                      @Param("min") Float min,
                      @Param("max") Float max,
                      @Param("isDate") Boolean isDate,
                      @Param("pageIndex") Integer pageIndex,
                      @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT count(*) FROM product a INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t3.id=t2.value_detail_id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (1=1) END) AND " +
		    "(CASE WHEN :brandId IS NOT NULL THEN a.brand_id=:brandId ELSE (1=1) END) AND " +
		    "(CASE WHEN :categoryId IS NOT NULL THEN a.category_id=:categoryId ELSE (1=1) END) AND " +
		    "(CASE WHEN :skinTypeId IS NOT NULL THEN t4.skin_type_id=:skinTypeId ELSE (1=1) END) AND " +
		    "(CASE WHEN (:min IS NOT NULL AND :max IS NOT NULL) THEN t3.sell_price BETWEEN :min and :max ELSE (1=1) END) AND " +
		    "(CASE WHEN :isDate IS NOT NULL THEN " +
		    	"(CASE WHEN :isDate=true THEN a.expiration_date < NOW() ELSE a.expiration_date >= NOW() END) " +
		    " ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.name REGEXP :keyword " +
		    "OR a.code REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) GROUP BY a.id", nativeQuery = true)
	List<Integer> cntProductBySkinType (@Param("keyword") String keyword,
					@Param("status") String status,
		            @Param("brandId") String brandId,
		            @Param("categoryId") String categoryId,
		            @Param("skinTypeId") String skinTypeId,
		            @Param("min") Float min,
                    @Param("max") Float max,
                    @Param("isDate") Boolean isDate);
	
	@Query(value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, " +
			"a.expiration_date AS expirationDate, a.category_id AS categoryId, a.brand_id AS brandId, " +
			"c.name AS categoryName, b.name AS brandName, min(t3.sell_price) AS minPrice, max(t3.sell_price) AS maxPrice, " +
			"GROUP_CONCAT(DISTINCT CONCAT(t5.id, ':', t5.name) ORDER BY t5.name) AS skinTypes " +
			"FROM product a INNER JOIN category c ON c.id=a.category_id " +
			"INNER JOIN brand b ON b.id=a.brand_id " +
			"INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"INNER JOIN skin_type t5 ON t5.id=t4.skin_type_id " +
			"WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (a.status <> '') END) AND " +
		    "(CASE WHEN :brandId IS NOT NULL THEN a.brand_id=:brandId ELSE (1=1) END) AND " +
		    "(CASE WHEN :categoryId IS NOT NULL THEN a.category_id=:categoryId ELSE (1=1) END) AND " +
		    "(CASE WHEN (:min IS NOT NULL AND :max IS NOT NULL) THEN t3.sell_price BETWEEN :min and :max ELSE (1=1) END) AND " +
		    "(CASE WHEN :isDate IS NOT NULL THEN " +
		    	"(CASE WHEN :isDate=true THEN a.expiration_date < NOW() ELSE a.expiration_date >= NOW() END) " +
		    " ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.name REGEXP :keyword " +
		    "OR a.code REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) " +
		    "GROUP BY a.id LIMIT :pageSize OFFSET :pageIndex",
		    nativeQuery = true)
	List<Object> search(@Param("keyword") String keyword,
						@Param("status") String status,
                      @Param("brandId") String brandId,
                      @Param("categoryId") String categoryId,
                      @Param("min") Float min,
                      @Param("max") Float max,
                      @Param("isDate") Boolean isDate,
                      @Param("pageIndex") Integer pageIndex,
                      @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT count(*) FROM product a INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (1=1) END) AND " +
		    "(CASE WHEN :brandId IS NOT NULL THEN a.brand_id=:brandId ELSE (1=1) END) AND " +
		    "(CASE WHEN :categoryId IS NOT NULL THEN a.category_id=:categoryId ELSE (1=1) END) AND " +
		    "(CASE WHEN (:min IS NOT NULL AND :max IS NOT NULL) THEN t3.sell_price BETWEEN :min and :max ELSE (1=1) END) AND " +
		    "(CASE WHEN :isDate IS NOT NULL THEN " +
	    		"(CASE WHEN :isDate=true THEN a.expiration_date < NOW() ELSE a.expiration_date >= NOW() END) " +
	    	" ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.name REGEXP :keyword " +
		    "OR a.code REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) GROUP BY a.id", nativeQuery = true)
	List<Integer> cntProduct (@Param("keyword") String keyword,
					@Param("status") String status,
		            @Param("brandId") String brandId,
		            @Param("categoryId") String categoryId,
		            @Param("min") Float min,
                    @Param("max") Float max,
                    @Param("isDate") Boolean isDate);
	
	
	@Query( value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, a.expiration_date AS expirationDate, " +
			"a.category_id AS categoryId, a.brand_id AS brandId, c.name AS categoryName, b.name AS brandName, " +
			"min(t3.sell_price) AS minPrice, max(t3.sell_price) AS maxPrice, sum(DISTINCT t3.sell_quantity) AS totalQuantity, " +
			"GROUP_CONCAT(DISTINCT CONCAT(t5.id, ':', t5.name) ORDER BY t5.name) AS skinTypes, " +
			"t3.id AS valueDetailId, t2.id AS productItemId, t2.value AS value, t3.import_price AS importPrice, " +
			"t3.sell_price AS sellPrice, t3.import_quantity AS importQuantity, t3.sell_quantity AS sellQuantity, " +
			"t3.status As valueStatus, t3.unit AS unit, t3.image AS image " +
			"FROM product a INNER JOIN category c ON c.id=a.category_id " +
			"INNER JOIN brand b ON b.id=a.brand_id " +
			"INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"INNER JOIN skin_type t5 ON t5.id=t4.skin_type_id " + 
			"WHERE t2.id=:id "
			, nativeQuery = true)
	Object getProductByProductItem(@Param("id") String id);
	
	@Query(value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, a.expiration_date AS expirationDate, " +
			"a.category_id AS categoryId, a.brand_id AS brandId, t.quantity AS quantityDiscount " +
			"FROM product a INNER JOIN product_discount t ON t.product_id=a.id " +
			"WHERE t.discount_id=:id"
			, nativeQuery = true )
	List<Object> getProductByDiscount(@Param("id") String id);
	
	
	@Query(value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, " +
			"a.expiration_date AS expirationDate, a.category_id AS categoryId, a.brand_id AS brandId, " +
			"c.name AS categoryName, b.name AS brandName, min(t3.sell_price) AS minPrice, max(t3.sell_price) AS maxPrice, " +
			"GROUP_CONCAT(DISTINCT CONCAT(t5.id, ':', t5.name) ORDER BY t5.name) AS skinTypes " +
			"FROM product a INNER JOIN category c ON c.id=a.category_id " +
			"INNER JOIN brand b ON b.id=a.brand_id " +
			"INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"INNER JOIN skin_type t5 ON t5.id=t4.skin_type_id " +
			"INNER JOIN favorite f ON f.product_id=a.id " +
			"WHERE f.user_id=:id GROUP BY a.id ORDER BY f.created_at DESC"
			, nativeQuery = true)
	List<Object> getProductFavoriteByUser(@Param("id") String id);
	
	
	
	@Query(value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, " +
			"a.expiration_date AS expirationDate, a.category_id AS categoryId, a.brand_id AS brandId, " +
			"c.name AS categoryName, b.name AS brandName, min(t3.sell_price) AS minPrice, max(t3.sell_price) AS maxPrice, " +
			"GROUP_CONCAT(DISTINCT CONCAT(t5.id, ':', t5.name) ORDER BY t5.name) AS skinTypes " +
			"FROM product a INNER JOIN category c ON c.id=a.category_id " +
			"INNER JOIN brand b ON b.id=a.brand_id " +
			"INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"INNER JOIN skin_type t5 ON t5.id=t4.skin_type_id " +
			"WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (a.status <> '') END) AND " +
		    "(CASE WHEN :brandId IS NOT NULL THEN a.brand_id=:brandId ELSE (1=1) END) AND " +
		    "(CASE WHEN :categoryId IS NOT NULL THEN a.category_id=:categoryId ELSE (1=1) END) AND " +
		    "(CASE WHEN (:min IS NOT NULL AND :max IS NOT NULL) THEN t3.sell_price BETWEEN :min and :max ELSE (1=1) END) AND " +
		    "(CASE WHEN :isDate IS NOT NULL THEN " +
		    	"(CASE WHEN :isDate=true THEN a.expiration_date < NOW() ELSE a.expiration_date >= NOW() END) " +
		    " ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.name REGEXP :keyword " +
		    "OR a.code REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) " +
		    "GROUP BY a.id ORDER BY a.created_at LIMIT :pageSize OFFSET :pageIndex",
		    nativeQuery = true)
	List<Object> newest(@Param("keyword") String keyword,
						@Param("status") String status,
                      @Param("brandId") String brandId,
                      @Param("categoryId") String categoryId,
                      @Param("min") Float min,
                      @Param("max") Float max,
                      @Param("isDate") Boolean isDate,
                      @Param("pageIndex") Integer pageIndex,
                      @Param("pageSize") Integer pageSize);
	
	
	@Query(value = "SELECT a.id AS id, a.code AS code, a.name AS name, a.photo AS photo, a.made_in AS madeIn, " +
			"a.status AS status, a.description AS description, a.production_date AS productionDate, " +
			"a.expiration_date AS expirationDate, a.category_id AS categoryId, a.brand_id AS brandId, " +
			"c.name AS categoryName, b.name AS brandName, min(t3.sell_price) AS minPrice, max(t3.sell_price) AS maxPrice, " +
			"GROUP_CONCAT(DISTINCT CONCAT(t5.id, ':', t5.name) ORDER BY t5.name) AS skinTypes " +
			"FROM product a INNER JOIN category c ON c.id=a.category_id " +
			"INNER JOIN brand b ON b.id=a.brand_id " +
			"INNER JOIN product_item t2 ON t2.product_id=a.id " +
			"INNER JOIN value_detail t3 ON t2.value_detail_id=t3.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=a.id " +
			"INNER JOIN skin_type t5 ON t5.id=t4.skin_type_id " +
			"WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN a.status=:status ELSE (a.status <> '') END) AND " +
		    "(CASE WHEN :brandId IS NOT NULL THEN a.brand_id=:brandId ELSE (1=1) END) AND " +
		    "(CASE WHEN :categoryId IS NOT NULL THEN a.category_id=:categoryId ELSE (1=1) END) AND " +
		    "(CASE WHEN :skinTypeId IS NOT NULL THEN t4.skin_type_id=:skinTypeId ELSE (1=1) END) AND " +
		    "(CASE WHEN (:min IS NOT NULL AND :max IS NOT NULL) THEN t3.sell_price BETWEEN :min and :max ELSE (1=1) END) AND " +
		    "(CASE WHEN :isDate IS NOT NULL THEN " +
		    	"(CASE WHEN :isDate=true THEN a.expiration_date < NOW() ELSE a.expiration_date >= NOW() END) " +
		    " ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(a.name REGEXP :keyword " +
		    "OR a.code REGEXP :keyword) " +
		    "ELSE (a.id IS NOT NULL) END) " +
		    "GROUP BY a.id ORDER BY a.created_at LIMIT :pageSize OFFSET :pageIndex",
		    nativeQuery = true)
	List<Object> searchBySkinTypeNewest(@Param("keyword") String keyword,
					@Param("status") String status,
                      @Param("brandId") String brandId,
                      @Param("categoryId") String categoryId,
                      @Param("skinTypeId") String skinTypeId,
                      @Param("min") Float min,
                      @Param("max") Float max,
                      @Param("isDate") Boolean isDate,
                      @Param("pageIndex") Integer pageIndex,
                      @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT sum(t3.quantity) FROM product t INNER JOIN product_item t2 ON t2.product_id=t.id " +
			"INNER JOIN order_item t3 ON t3.product_item_id=t2.id WHERE t.id=:id"
			, nativeQuery = true)
	Integer cntTotalSellByProduct(@Param("id") String id);
	
	@Query(value = "SELECT sum(t3.quantity) FROM product t INNER JOIN product_item t2 ON t2.product_id=t.id " +
			"INNER JOIN order_item t3 ON t3.product_item_id=t2.id WHERE t.brand_id=:id "
			, nativeQuery = true)
	Integer cntTotalSellByBrand(@Param("id") String id);
	
	@Query(value = "SELECT sum(t3.quantity) FROM product t INNER JOIN product_item t2 ON t2.product_id=t.id " +
			"INNER JOIN order_item t3 ON t3.product_item_id=t2.id " +
			"INNER JOIN product_skin_type t4 ON t4.product_id=t.id" +
			" WHERE t4.skin_type_id=:id "
			, nativeQuery = true)
	Integer cntTotalSellBySkinType(@Param("id") String id);
	
	@Query(value = "SELECT sum(t3.quantity) FROM product t INNER JOIN product_item t2 ON t2.product_id=t.id " +
			"INNER JOIN order_item t3 ON t3.product_item_id=t2.id WHERE t.category_id=:id "
			, nativeQuery = true)
	Integer cntTotalSellByCategory(@Param("id") String id);
}
