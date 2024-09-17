package com.cosmetic.gg.repository.product;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.product.Review;

@Repository
public interface ReviewRepository extends JpaRepository<Review, String>{

	@Query(value = "SELECT t.id AS id, t.user_id AS userId, t.product_item_id AS productItemId, " +
			"t.comment AS commnet, t.star AS star, t.created_at AS createdAt, t2.value AS value " +
			"FROM review t INNER JOIN product_item t2 ON t2.id=t.product_item_id " +
			"WHERE (CASE WHEN :star IS NOT NULL THEN t.star=:star ELSE (1=1) END) AND " +
			"t2.product_id=:id ORDER BY t.created_at DESC", nativeQuery = true)
	List<Object> search(@Param("id") String id, @Param("star") Integer star);
	
	@Query(value = "SELECT SUM(t.star) FROM review t INNER JOIN product_item t2 ON t2.id=t.product_item_id "
			+ "WHERE t2.product_id=:id", nativeQuery = true)
	Integer cntStarByProduct(@Param("id") String id);
	
	@Query(value = "SELECT COUNT(*) FROM review t INNER JOIN product_item t2 ON t2.id=t.product_item_id "
			+ "WHERE (t2.product_id=:id AND t.star IS NOT NULL)", nativeQuery = true)
	Integer cntReviewByProduct(@Param("id") String id);
}
