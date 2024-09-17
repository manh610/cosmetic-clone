package com.cosmetic.gg.repository.product;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.cosmetic.gg.entity.product.Category;


@Repository
public interface CategoryRepository extends JpaRepository<Category, String> {

	@Query(value = "SELECT * FROM category t WHERE (t.id = :key OR t.code = :key OR t.name = :key)", nativeQuery = true)
	Category findByKey(@Param("key") String key);
	
	@Query(value = "SELECT * FROM category t WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN t.status=:status ELSE (t.status='ACTIVE' or t.status='DELETED') END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(t.code REGEXP :keyword OR t.name REGEXP :keyword) " +
		    "ELSE (t.id IS NOT NULL) END) AND " +
		    "(CASE WHEN :parentId IS NOT NULL AND :parentId <> '' THEN " +
		    "(t.ancestors LIKE CONCAT('%',:parentId,'%')) " +
		    "ELSE (t.id IS NOT NULL) END) " +
		    "ORDER BY t.offset ASC, t.parent_id ASC, t.name ASC, t.created_at DESC LIMIT :pageSize OFFSET :pageIndex",
		    nativeQuery = true)
	List<Category> search(@Param("keyword") String keyword,
					  @Param("parentId") String parentId,
                      @Param("status") String status,
                      @Param("pageIndex") Integer pageIndex,
                      @Param("pageSize") Integer pageSize);
	
	@Query(value = "SELECT count(*) FROM category t WHERE " +
			"(CASE WHEN :status IS NOT NULL THEN t.status=:status ELSE (1=1) END) AND " +
		    "(CASE WHEN :keyword IS NOT NULL AND :keyword <> '' THEN " +
		    "(t.code REGEXP :keyword OR t.name REGEXP :keyword) " +
		    "ELSE (t.id IS NOT NULL) END) AND " +
		    "(CASE WHEN :parentId IS NOT NULL AND :parentId <> '' THEN " +
		    "(t.ancestors LIKE CONCAT('%',:parentId,'%')) " +
		    "ELSE (t.id IS NOT NULL) END)" 
		    , nativeQuery = true)
	Integer cntCategory(@Param("keyword") String keyword,
					  @Param("parentId") String parentId,
                      @Param("status") String status);
	
	
	
	@Query(value = "SELECT * FROM category t WHERE t.ancestors LIKE CONCAT('%',:parentId,'%') " +
		    "ORDER BY t.parent_id ASC, t.offset, t.name ASC, t.created_at DESC", nativeQuery = true)
	List<Category> allChildren(@Param("parentId") String parentId);
	
	@Query(value = "SELECT * FROM category t WHERE " +
		    "(CASE WHEN :parentId IS NOT NULL AND :parentId <> '' THEN " +
		    "t.parent_id = :parentId ELSE (t.parent_id IS NULL OR t.parent_id = '') END) " +
		    "ORDER BY t.parent_id ASC, t.offset, t.name ASC, t.created_at DESC", nativeQuery = true)
	List<Category> children(@Param("parentId") String parentId);
	
	@Transactional
	@Modifying
	@Query(value = "UPDATE category SET ancestors = " +
	    "(CASE WHEN :oldAncestors IS NOT NULL AND :oldAncestors <> '' THEN " +
	    "REPLACE(REPLACE(ancestors, CONCAT(:oldAncestors, ';'), :newAncestors ), CONCAT(';', :oldAncestors), :newAncestors) " +
	    "ELSE CONCAT(:newAncestors, ';', ancestors) END) " +
	    "WHERE ancestors LIKE CONCAT('%',:parentId,'%')", nativeQuery = true)
	void bulkUpdateAncestorChildren(@Param("parentId") String parentId,
                                  @Param("oldAncestors") String oldAncestors,
                                  @Param("newAncestors") String newAncestors);
	
	@Query(value = "SELECT * FROM category t WHERE " +
		    "(t.parent_id IS NULL OR  t.parent_id = '') " +
			"AND t.status='ACTIVE' " +
		    "ORDER BY t.code ASC, t.name ASC, t.created_at DESC", nativeQuery = true)
	List<Category> allRoot();
}
