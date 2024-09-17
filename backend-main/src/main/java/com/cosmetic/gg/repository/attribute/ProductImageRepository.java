package com.cosmetic.gg.repository.attribute;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.attribute.ProductImage;

@Repository
public interface ProductImageRepository extends JpaRepository<ProductImage, String>{

	@Query(value = "SELECT * FROM product_image a WHERE (a.id=:key OR a.product_id=:key)", nativeQuery = true)
	List<ProductImage> findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM product_image a WHERE (a.id=:id OR a.product_id=:id)", nativeQuery = true)
	List<ProductImage> findImage(@Param("id") String id);
	
	@Query(value = "SELECT * FROM product_image t INNER JOIN product_item t2 ON t2.product_id=t.product_id " +
			"WHERE t2.id=:id"
			, nativeQuery = true)
	List<ProductImage> findByProductItem(@Param("id") String id);
	
	@Query(value = "SELECT * FROM product_image t WHERE t.review_id=:id", nativeQuery = true)
	List<ProductImage> findByReview(@Param("id") String id);
}
